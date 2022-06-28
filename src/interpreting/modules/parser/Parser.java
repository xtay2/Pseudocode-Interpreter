package interpreting.modules.parser;

import static misc.supporting.Output.*;

import java.util.*;

import formatter.basic.Formatter;
import importing.*;
import importing.filedata.paths.*;
import interpreting.modules.assembler.*;
import launching.*;
import misc.util.*;

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
