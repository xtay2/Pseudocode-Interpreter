package interpreting.modules.parser;

import static misc.helper.StringHelper.indexLines;
import static misc.supporting.Output.print;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import formatter.basic.Formatter;
import importing.Importer;
import interpreting.modules.disassembler.Disassembler;
import launching.Main;
import misc.supporting.FileManager;

public final class Parser {

	/**
	 * An indexed line is a String with the orgLineNr attached.
	 *
	 * @param line is the text.
	 * @param index is the lineNr from the editor.
	 */
	public record IdxLine(String line, int index) {

		@Override
		public String toString() {
			return index + ": " + line;
		}

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
	public static void parse(Path mainFilePath, boolean forceFormat) {
		// TODO Format only relevant files
		List<IdxLine> lines = indexLines(format(mainFilePath));
		Importer.test();

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
