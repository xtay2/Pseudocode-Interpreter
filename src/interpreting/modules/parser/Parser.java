package interpreting.modules.parser;

import static misc.supporting.Output.print;
import static misc.supporting.Output.printAll;

import java.util.List;

import formatter.basic.Formatter;
import importing.Importer;
import importing.filedata.paths.DataPath;
import interpreting.modules.assembler.Assembler;
import launching.Main;
import misc.util.Tuple;

public final class Parser {

	/**
	 * Converts the String to a Program.
	 *
	 * @param lineArray
	 * @return
	 */
	public static void parse(boolean forceFormat) {
		Formatter.formatAll(forceFormat);

		List<Tuple<DataPath, String>> lines = Importer.getLines();

		print("Assembling...");
		lines = Assembler.assemble(lines);

		// At this point, all lines are stripped.
		printAll("Imported Program", lines);

		// Index all newly written and formatted lines correctly, as this is the code
		// that the user sees.
		for (Tuple<DataPath, String> line : lines)
			Main.PROGRAM.appendLine(line.val2, line.val1);
		Main.PROGRAM.constructAndMerge();
	}
}
