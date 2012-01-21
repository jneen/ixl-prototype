# encoding: UTF-8

require 'rubygems'
require 'rltk/lexer'
require 'rltk/parser'
require 'rltk/ast'

module Ixl
  class Lexer < RLTK::Lexer
    rule(/\[/)        { [:OPEN_MACRO, 'lambda'] }
    rule(/\.\[/)      { [:OPEN_MACRO, 'eval'] }
    rule(/\.(\w*)\[/) { |m| [:OPEN_MACRO, m[1..-2]] }
    rule(/\.\w*/)     { |v| [:VAR, v[1..-1]] }
    rule(/\|/)        { :PIPE }
    rule(/\]/)        { :CLOSE }
    rule(/;\n/)       { :TERM }
    rule(/\s+/)
    rule(/[^\[\]\|;\s]*/) { |s| [:STRING, s] }
  end

  class Parser < RLTK::Parser
    production :program do
      clause('chain') { |c| AST::Program.new([c]) }
      clause('program TERM chain TERM') do |p, _, c, _|
        p.chains << c
        p
      end
    end

    production :chain do
      clause('command opt_terms') { |c, _| AST::Chain.new([c]) }
      clause('chain PIPE command') { |ch, _, c| ch.commands << c; ch }
    end

    production :command do
      clause('STRING') { |x| AST::Command.new(x, []) }
      clause('STRING expr_seq') { |x, s| AST::Command.new(x, s) }
    end

    production :expr_seq do
      clause('expr') { |x| [x] }
      clause('expr_seq expr') { |s, x| s << x }
    end

    production :expr do
      clause('STRING') { |s| AST::StringNode.new(s) }
      clause('VAR') { |v| AST::Variable.new(v) }
      clause('OPEN_MACRO program CLOSE') { |macro, body, _| AST::Macro.new(macro, body) }
    end

    # production(:terms) do
    #   clause('LINE_ENDING+') { |_| [] }
    # end

    production(:opt_terms) do
      clause('') { nil }
      clause('opt_terms TERM') { |_,_| }
    end

    finalize
  end

  module AST
    class Base < RLTK::ASTNode
    end

    class Expression < Base
    end

    class StringNode < Expression
      value :string, String
    end

    class Variable < Expression
      value :name, String
    end

    class Command < Base
      value :command, String
      child :args, [Expression]
    end

    class Chain < Base
      child :commands, [Command]
    end

    class Program < Base
      child :chains, [Chain]
    end

    class Macro < Expression
      value :name, String
      child :body, Program
    end
  end

  class Environment
    attr_accessor :it
    attr_accessor :parent
    attr_accessor :locals
    attr_accessor :args
    def initialize(it, parent, locals={}, args=[])
      @it = it
      @parent = parent
      @locals = locals
      @args = args
    end

    def self.base
      @base ||= new(nil, nil, {
        'lambda' => proc { |context, expr|
          closure = context.sub
          proc { |context, args|
            closure.args = args
            closure.eval(expr)
          }
        },
        'eval' => proc { |context, expr|
          context.sub.eval(expr)
        },
        'set' => proc { |context, args|
          var, val = args
          context[var] = val
          val
        },
        'shift' => proc { |context, args|
          context.args.shift
        },
        ':' => proc { |context, args|
          args.last
        },
        'add' => proc { |context, args|
          args.map(&:to_f).inject(&:+).to_s
        },
        'mul' => proc { |context, args|
          args.map(&:to_f).inject(&:*).to_s
        },
      })
    end

    def [](k)
      locals[k.to_s] or parent && parent[k]
    end

    def key?(k)
      local_key?(k) or parent && parent.key?(k)
    end

    def local_key?(k)
      locals.key?(k.to_s)
    end

    def []=(k, v)
      k = k.to_s
      locals[k] = v
    end

    def sub(_it=nil)
      _it ||= self.it
      Environment.new(_it, self)
    end

    def check_defined!(name)
      raise "undefined: #{name}" unless key?(name)
    end

    def check_callable!(obj)
      raise "not callable: #{obj}" unless obj.respond_to? :call
    end

    def eval(node)
      case node
      when AST::Variable
        # special case plain .
        if node.name == ''
          self.it
        else
          check_defined!(node.name)
          self[node.name]
        end
      when AST::Macro
        check_defined!(node.name)
        macro = self[node.name]
        check_callable!(macro)
        macro.call(self, node.body)
      when AST::Command
        check_defined!(node.command)
        cmd = self[node.command]
        check_callable!(cmd)

        cmd.call(self, node.args.map { |a| self.eval(a) })
      when AST::StringNode
        node.string
      when AST::Chain
        _it = self.it

        result = node.commands.map do |chain|
          self.it = self.eval(chain)
        end.last

        self.it = _it

        result
      when AST::Program
        node.chains.map do |chain|
          self.eval(chain)
        end.last
      else
        raise "not an ixl program: #{node}"
      end
    end
  end

  class << self
    def shell(env, prompt=nil)
      print prompt if prompt
      $stdin.each do |line|
        lexed = Lexer.lex(line)
        # puts "LEXED:"
        # p lexed

        begin
          $last_parsed = parsed = Parser.parse(lexed)
          p env.eval(parsed)
        rescue Exception => e
          puts e
          puts e.backtrace
        end

        print prompt
      end

      puts
    end
  end
end

if __FILE__ == $0
  env = Ixl::Environment.base.sub

  if $stdin.tty?
    Ixl.shell(env, ':> ')
  else
    Ixl.shell(env)
  end
end
