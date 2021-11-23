package programreader.parser;

import programreader.interpreter.Interpreter;
import programreader.program.Program;
import static helper.Output.*;

public class Parser {

	private static Program program = new Program();

	public static final char SINGLE_LINE_COMMENT = '#';
	
	public static void parseToExpressions(String[] lineArray) {
		initProgram(lineArray);
		Interpreter.interpret(program);
	}

	private static void initProgram(String[] lineArray) {
		int i = 0;
		print("Raw Expressions:" + UNDERLINE);
		for (String line : lineArray) {
			if (!line.isBlank() && line.charAt(0) != SINGLE_LINE_COMMENT) {
				program.writeLine(i, line.strip());
				i++;
			}
		}
		print(LINE_BREAK);
	}
}
