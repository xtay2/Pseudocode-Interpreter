package parsing.parser;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import helper.FileManager;
import main.Main;
import parsing.codeformatter.Formatter;
import parsing.importer.Importer;
import parsing.program.Program;

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
		List<String> lines = Formatter.format(new ArrayList<>(Files.readAllLines(Path.of(Main.filePath))));
		FileManager.writeFile(lines, Main.filePath);
		//Import everything possibly needed.
		lines = Importer.importData(lines);
		//Remove every function-declaration that doesn't get called.
		lines = Disassembler.disassemble(lines);
		Program program = new Program();
		for (String line : lines)
			program.appendLine(line.strip());
		return program;
	}
}
