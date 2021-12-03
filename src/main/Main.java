package main;

import interpreter.Interpreter;
import parser.Parser;
import parser.program.Program;

public class Main {

	public static Program program;

	public static void main(String[] args) {
		program = Parser.parse("Files/Program.txt");
		Interpreter.interpret(program);
	}
}