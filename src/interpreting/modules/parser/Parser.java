package interpreting.modules.parser;

import static misc.supporting.Output.print;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import formatter.basic.Formatter;
import importing.Importer;
import importing.filedata.paths.DataPath;
import interpreting.modules.assembler.Assembler;
import launching.Main;
import misc.helper.StringHelper;
import misc.supporting.FileManager;
import misc.util.Tuple;

public final class Parser {

	/**
	 * An indexed line is a String with the orgLineNr attached.
	 *
	 * @param line is the text.
	 * @param index is the lineNr from the editor.
	 */
	@Deprecated
	public record IdxLine(String line, int index) {

		@Override
		public String toString() {
			return index + ": " + line;
		}
	}

	/**
	 * Formats the file and returns the result.
	 */
	@Deprecated
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
		// List<IdxLine> lines = indexLines(format(mainFilePath));
		List<Tuple<DataPath, String>> lines = Importer.getLines();

		System.out.println(StringHelper.enumerate(lines));

		lines = Assembler.assemble(lines);

		// At this point, all lines are stripped.
		print(StringHelper.enumerate(lines));

		// Index all newly written and formatted lines correctly, as this is the code
		// that the user sees.
		for (Tuple<DataPath, String> line : lines)
			Main.PROGRAM.appendLine(line.val2, line.val1);
		Main.PROGRAM.constructAndMerge();
	}
}
