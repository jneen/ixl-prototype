# encoding: UTF-8

require 'rubygems'
require 'rltk/lexer'
require 'rltk/parser'
require 'rltk/ast'

module Ixl
  class Lexer < RLTK::Lexer
    rule(/\[/)        { [:OPEN_MACRO, 'lambda'] }
    rule(/\.\[/)      { [:OPEN_MACRO, 'eval'] }
    rule(/\.(\w*)\[/) { |m| m =~ /\.(\w*)\[/; [:OPEN_MACRO, $1] }
    rule(/\.\w*/)     { |v| v =~ /\.(\w*)/; [:VAR, $1] }
    rule(/\|/)        { :PIPE }
    rule(/\]/)        { :CLOSE }
    rule(/;\n/)         { :TERM }
    rule(/\s+/)
    rule(/[^\[\]\|;\s]*/) { |s| [:STRING, s] }
  end

  class Parser < RLTK::Parser
    production :program do
      clause('chain opt_terms') { |c, _| AST::Program.new([c]) }
      clause('program TERM chain opt_terms') do |p, _, c, _|
        p.chains << c
        p
      end
    end

    production :chain do
      clause('command') { |c| AST::Chain.new([c]) }
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
      clause('OPEN_MACRO chain CLOSE') { |macro, command, _| AST::Macro.new(macro, command) }
    end

    # production(:terms) do
    #   clause('LINE_ENDING+') { |_| [] }
    # end

    production(:opt_terms) do
      clause('') { nil }
      clause('opt_terms TERM') { |_,_| }
    end

    finalize :explain => true
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

    class Macro < Expression
      value :name, String
      child :body, Chain
    end

    class Program < Base
      child :chains, [Chain]
    end
  end
end

def run
  print ':> '
  $stdin.each do |line|
    lexed = Ixl::Lexer.lex(line)
    puts "LEXED:"
    p lexed

    begin
      puts "=================== parsing ==================="
      $last_parsed = parsed = Ixl::Parser.parse(lexed, :verbose => true)
      puts "================= done parsing ================"
      puts "PARSED:"
      p parsed
    rescue Exception => e
      puts e
    end

    print ':> '
  end

  puts
end

run if ARGV.include? 'run'
