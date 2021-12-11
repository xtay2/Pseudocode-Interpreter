package parser;

import static helper.Output.LINE_BREAK;
import static helper.Output.UNDERLINE;
import static helper.Output.print;

import codeformatter.Formatter;
import helper.FileManager;
import parser.program.Program;

public final class Parser {

	public static final char SINGLE_LINE_COMMENT = '#';

	/**
	 * Converts the String to a Program.
	 *
	 * @param lineArray
	 * @return
	 */
	public static Program parse(String path) {
		String[] lineArray = Formatter.format(FileManager.fileToLineArray(path));
		FileManager.writeFile(lineArray, path);
		int i = 0;
		Program program = new Program();
		print("Raw Expressions:" + UNDERLINE);
		for (String line : lineArray)
			if (!line.isBlank() && line.charAt(0) != SINGLE_LINE_COMMENT) {
				program.writeLine(i, line.strip());
				i++;
			}
		print(LINE_BREAK);
		return program;
	}
}
