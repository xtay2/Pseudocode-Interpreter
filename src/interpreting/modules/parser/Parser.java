package interpreting.modules.parser;

import static misc.helper.Output.print;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import formatter.basic.Formatter;
import interpreting.modules.disassembler.Disassembler;
import launching.Main;
import misc.helper.FileManager;

public final class Parser {

	public static final char SINGLE_LINE_COMMENT = '#';
	public static final char MULTI_CLOSE_SCOPE = ';';

	/**
	 * An indexed line is a String with the orgLineNr attached.
	 * 
	 * @param line  is the text.
	 * @param index is the lineNr from the editor.
	 */
	public record IdxLine(String line, int index) {

		@Override
		public String toString() {
			return index + ": " + line;
		}

	}

	public static List<IdxLine> indexLines(List<String> lines) {
		List<IdxLine> indexedLines = new ArrayList<>();
		for (int i = 0; i < lines.size(); i++)
			indexedLines.add(new IdxLine(lines.get(i), i + 1));
		return indexedLines;
	}

	/**
	 * Formats the file and returns the result.
	 */
	private static List<String> format(Path mainFilePath) {
		List<String> lines;
		try {
			lines = Files.readAllLines(mainFilePath, StandardCharsets.UTF_8);
			// Format all lines.
			lines = Formatter.format(lines, true);

			// Write the formatted lines back into the file.
			FileManager.writeFile(lines, mainFilePath);

			return lines;
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(0);
		}
		return null;
	}

	/**
	 * Converts the String to a Program.
	 *
	 * @param lineArray
	 * @return
	 */
	public static void parse(Path libPath, Path mainFilePath, boolean forceFormat) {
		List<IdxLine> lines = indexLines(format(mainFilePath));

		lines = Disassembler.dissassemble(lines);

		// At this point, all lines are stripped.
		lines.stream().forEach(e -> print(e));

		print("-".repeat(70));

		// Index all newly written and formatted lines correctly, as this is the code
		// that the user sees.
		for (IdxLine line : lines)
			Main.PROGRAM.appendLine(line.line(), line.index());
		Main.PROGRAM.constructAndMerge();
	}
}
