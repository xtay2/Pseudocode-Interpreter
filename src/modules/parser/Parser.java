package modules.parser;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import helper.FileManager;
import main.Main;
import modules.formatter.Formatter;
import modules.importer.Importer;

public final class Parser {

	public record LineInfo(String line, int index) {
	}

	public static final char SINGLE_LINE_COMMENT = '#';

	public static List<LineInfo> indexLines(List<String> lines) {
		List<LineInfo> indexedLines = new ArrayList<>();
		for (int i = 0; i < lines.size(); i++)
			indexedLines.add(new LineInfo(lines.get(i), i + 1));
		return indexedLines;
	}

	/**
	 * Converts the String to a Program.
	 *
	 * @param lineArray
	 * @return
	 * @throws IOException
	 */
	public static void parse() throws IOException {
		List<String> lines = Files.readAllLines(Path.of(Main.filePath));

		// Format all lines.
		lines = Formatter.format(lines);

		// Write the formatted lines back into the file.
		FileManager.writeFile(lines, Main.filePath);

		// Index all newly written and formatted lines correctly, as this is the code
		// that the user sees.
		List<LineInfo> indexedLines = indexLines(lines);

		// Import everything possibly needed.
		indexedLines = Importer.importData(indexedLines);

		// Remove every function-declaration that doesn't get called.
		indexedLines = Disassembler.disassemble(indexedLines);

		for (LineInfo line : indexedLines)
			Main.PROGRAM.appendLine(line.line(), line.index());
		Main.PROGRAM.constructAndMerge();
	}
}
