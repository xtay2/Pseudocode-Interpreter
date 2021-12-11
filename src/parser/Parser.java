package parser;

import static helper.Output.LINE_BREAK;
import static helper.Output.UNDERLINE;
import static helper.Output.print;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import helper.FileManager;
import main.Main;
import parser.program.Program;

public final class Parser {

	public static final char SINGLE_LINE_COMMENT = '#';

	public static final String importKeyword = "import";

	public static final ArrayList<String> importedFiles = new ArrayList<>();

	/**
	 * Converts the String to a Program.
	 *
	 * @param lineArray
	 * @return
	 * @throws IOException
	 */
	public static Program parse(String path) throws IOException {
		ArrayList<String> lines = new ArrayList<>(Files.readAllLines(Path.of(path)));
		FileManager.writeFile(lines, path);
		lines = importFile(lines);
		int i = 0;
		Program program = new Program();
		print("Raw Expressions:" + UNDERLINE);
		for (String line : lines) {
			if (!line.isBlank() && line.charAt(0) != SINGLE_LINE_COMMENT) {
				program.writeLine(i, line.strip());
				i++;
			}
		}
		print(LINE_BREAK);
		return program;
	}

	private static ArrayList<String> importFile(ArrayList<String> lines) throws IOException {
		for (int i = 0; i < lines.size(); i++) {
			String l = lines.get(i);
			if (l.startsWith("import")) {
				lines.remove(i);
				if (!importedFiles.contains(l)) {
					print("Importing: " + l);
					importedFiles.add(l);
					String importPath = Main.WORKSPACE + l.substring(importKeyword.length()).strip().replace('.', '\\') + ".txt";
					ArrayList<String> imported = new ArrayList<String>(Files.readAllLines(Paths.get(importPath)));
					lines.addAll(i, importFile(imported));
				}
			}
		}
		return lines;
	}
}
