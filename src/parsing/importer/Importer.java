package parsing.importer;

import static helper.Output.print;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import exceptions.parsing.ImportingException;
import parsing.parser.Parser.LineInfo;
import parsing.program.KeywordType;

public abstract class Importer {

	private static final List<String> IMPORTED = new ArrayList<>();

	private static List<LineInfo> fetchFile(String importPath) {
		List<LineInfo> lines = null;
		try {
			List<String> imported = new ArrayList<String>(Files.readAllLines(Paths.get(importPath)));
			lines = importData(new ArrayList<>(imported.stream().map(l -> new LineInfo(l, -1)).toList()));
		} catch (IOException e) {
			throw new ImportingException(-1, importPath + " wasn't found or couldn't get opened.");
		}
		return lines;
	}

	public static List<LineInfo> importData(List<LineInfo> lines) {
		while (lines.get(0).line().startsWith(KeywordType.IMPORT.keyword)) {
			String currentImport = lines.remove(0).line().substring(KeywordType.IMPORT.keyword.length()).stripLeading();
			if (!IMPORTED.contains(currentImport)) {
				print("Importing: " + currentImport);
				IMPORTED.add(currentImport);
				lines.addAll(fetchFile(currentImport.replace('.', '\\') + ".pc"));
			}
		}
		return lines;
	}
}
