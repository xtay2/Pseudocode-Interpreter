package main;

import java.io.IOException;

import interpreter.Interpreter;
import parser.Parser;
import parser.program.Program;

public class Main {

	public static Program program;

	public static final String WORKSPACE = "C:\\Users\\x-tay\\git\\Pseudocode-II\\Workspace\\";
	
	public static void main(String[] args) throws IOException {
		try {
			String path = WORKSPACE + args[0] + ".txt";
			program = Parser.parse(path);
		} catch (IOException e) {
			e.printStackTrace();
		}
		Interpreter.interpret(program);
	}
}