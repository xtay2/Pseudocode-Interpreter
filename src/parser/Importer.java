package parser;

import static helper.Output.print;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;

public abstract class Importer {
	
	public static final String importKeyword = "import";
	
	private static final ArrayList<String> importedFiles = new ArrayList<>();
	
	static ArrayList<String> importFile(ArrayList<String> lines) throws IOException {
		for (int i = 0; i < lines.size(); i++) {
			String l = lines.get(i);
			if (l.startsWith("import")) {
				lines.remove(i);
				if (!importedFiles.contains(l)) {
					print("Importing: " + l);
					importedFiles.add(l);
					String importPath = l.substring(importKeyword.length()).strip().replace('.', '\\') + ".pc";
					ArrayList<String> imported = new ArrayList<String>(Files.readAllLines(Paths.get(importPath)));
					lines.addAll(i, importFile(imported));
				}
			}
		}
		return lines;
	}
}
