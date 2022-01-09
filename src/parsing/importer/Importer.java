package parsing.importer;

import static helper.Output.print;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import parsing.program.KeywordType;

public abstract class Importer {
		
	private static final ArrayList<String> importedFiles = new ArrayList<>();
	
	public static List<String> importData(List<String> lines) {
		for (int i = 0; i < lines.size(); i++) {
			String l = lines.get(i);
			if (l.startsWith("import")) {
				lines.remove(i);
				if (!importedFiles.contains(l)) {
					print("Importing: " + l);
					importedFiles.add(l);
					String importPath = l.substring(KeywordType.IMPORT.keyword.length()).strip().replace('.', '\\') + ".pc";
					try {
						ArrayList<String> imported = new ArrayList<String>(Files.readAllLines(Paths.get(importPath)));
						lines.addAll(i, importData(imported));
					}catch (IOException e) {
						return lines;
					}
				}
			}
		}
		return lines;
	}
}
