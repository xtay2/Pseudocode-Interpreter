package parser;

import static helper.Output.LINE_BREAK;
import static helper.Output.UNDERLINE;
import static helper.Output.print;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;

import codeformatter.Formatter;
import exceptions.parsing.IllegalCodeFormatException;
import main.Main;
import parser.program.Program;

public final class Parser {

	public static final char SINGLE_LINE_COMMENT = '#';

	/**
	 * Converts the String to a Program.
	 *
	 * @param lineArray
	 * @return
	 * @throws IOException
	 */
	public static Program parse() throws IOException {
		ArrayList<String> lines = Formatter.format(new ArrayList<>(Files.readAllLines(Path.of(Main.filePath))));
//		FileManager.writeFile(lines, Main.filePath);
		lines = Importer.importFile(lines);
		Program program = new Program();
		print("Raw Expressions:" + UNDERLINE);
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i);
			int lineBreak = line.indexOf(":");
			if (lineBreak != -1 && isNotInString(lineBreak, line)) {
				if (lineBreak == line.stripTrailing().length() - 1)
					throw new IllegalCodeFormatException("This one-line statement has to end with a semicolon.");
				//Ersetze Semikolon
				if (line.charAt(line.length() - 1) == ';') {
					line = line.substring(0, line.length() - 1);
					lines.add(i + 1, "}");
				}
				lines.add(i + 1, line.substring(lineBreak + 1)); // Teil nach :
				line = line.substring(0, lineBreak) + " {";
				lines.set(i, line);
			}
			if (!line.isBlank() && line.stripIndent().charAt(0) != SINGLE_LINE_COMMENT)
				program.appendLine(line.strip());

		}
		print(LINE_BREAK);
		return program;
	}

	/**
	 * Tells, if a char at a specified index is not in the string boundaries. ("")
	 * 
	 * @return true if the index is not in a string.
	 */
	private static boolean isNotInString(int index, String line) {
		boolean inString = false;
		for (int i = 0; i < index; i++) {
			if (inString && line.charAt(i) == '\\')
				i++;
			else if (line.charAt(i) == '"')
				inString = !inString;
		}
		return !inString && index != -1;
	}
}
