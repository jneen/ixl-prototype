# encoding: UTF-8

require 'rubygems'
require 'rltk/lexer'
require 'rltk/parser'
require 'rltk/ast'

module Ixl
  class Lexer < RLTK::Lexer
    rule(/#.*?$/)         { :TERM } # comments are at EOL

    rule(/\[/) { [:MACRO, 'lambda'] }
    rule(/\.\[/) { [:MACRO, 'eval'] }
    rule(/\.(\w*)\[/) { |m| [:MACRO, m[1..-2]] }
    rule(/\.\w*/) { |v| [:VAR, v[1..-1]] }
    rule(/\|/) { :PIPE }
    rule(/\]/) { :CLOSE }
    rule(/\s*([;\n]\s*)+/m) { :TERM }
    rule(/\s+/) { :WHITESPACE }
    rule(/[^\[\]\|;\s]*/) { |s| [:STRING, s] }
  end

  class Parser < RLTK::Parser
    production :program do
      clause('TERM* WHITESPACE* chain_seq WHITESPACE* TERM*') { |_,_,c,_,_| AST::Program.new(c) }
    end

    production :chain_seq do
      clause('chain') { |c| [c] }
      clause('chain_seq TERM+ chain') do |s, _, c|
        s + [c]
      end
    end

    production :chain do
      clause('command') { |c| AST::Chain.new([c]) }

      # pipes are allowed to continue on the second line
      # foo bar
      #   | baz
      #   | zot
      clause('chain TERM* WHITESPACE* PIPE WHITESPACE* command') do |ch,_,_,_,_,c|
        AST::Chain.new(ch.commands + [c])
      end
    end

    production :command do
      clause('expr') { |x| AST::Command.new(x, []) }
      clause('command WHITESPACE expr') do |c,_,x|
        AST::Command.new(c.command, c.args + [x])
      end
    end

    production :expr do
      clause('STRING') { |s| AST::StringNode.new(s) }
      clause('VAR') { |v| AST::Variable.new(v) }
      clause('MACRO program CLOSE') { |m, b, _| AST::Macro.new(m, b) }
    end

    finalize :explain => $DEBUG
  end

  module AST
    class Base < RLTK::ASTNode
    end

    class Expression < Base
    end

    class StringNode < Expression
      value :string, String

      def to_s
        string
      end
    end

    class Variable < Expression
      value :name, String

      def to_s
        ".#{name}"
      end
    end

    class Command < Base
      child :command, Expression
      child :args, [Expression]

      def to_s
        "#{command} #{args.map(&:to_s).join(' ')}"
      end
    end

    class Chain < Base
      child :commands, [Command]

      def to_s
        commands.join(' | ')
      end
    end

    class Program < Base
      child :chains, [Chain]

      def to_s
        chains.join("\n")
      end
    end

    class Macro < Expression
      value :name, String
      child :body, Program

      def to_s
        ".#{name}[#{body}]"
      end
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
        'args' => proc { |context, args|
          context.args
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
        'puts' => proc { |context, args|
          puts args.join(' ')
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
        # strings as commands cause an env lookup,
        # otherwise assumed it's a lambda
        cmd = case node.command
        when AST::StringNode
          check_defined!(node.command.string)
          self[node.command]
        else
          self.eval(node.command)
        end

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
        eval_string(line)
        print prompt
      end

      puts
    end

    def eval_string(str, env=nil)
      env ||= Environment.base.sub

      if $DEBUG
        puts "PROGRAM:"
        puts str
      end

      lexed = Lexer.lex(str)

      if $DEBUG
        puts "LEXED:"
        p lexed
      end

      begin
        parsed = Parser.parse(lexed, verbose: $DEBUG)
        p env.eval(parsed)
      rescue Exception => e
        puts e
        puts e.backtrace
      end

    end
  end
end

if __FILE__ == $0
  env = Ixl::Environment.base.sub

  if $stdin.tty?
    Ixl.shell(env, ':> ')
  else
    Ixl.eval_string($stdin.read)
  end
end
