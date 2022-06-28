package importing;

import static building.types.specific.KeywordType.*;
import static importing.filedata.paths.Location.*;

import java.io.*;
import java.util.*;

import building.expressions.main.functions.*;
import errorhandeling.*;
import importing.filedata.File;
import importing.filedata.interactable.*;
import importing.filedata.paths.*;
import misc.util.*;

/**
 * The class that is controlling all imports.
 *
 * <pre>
 * -All methods in this class should be static.
 * -This class mustn't have sublasses.
 * </pre>
 */
public abstract class Importer {
	
	private static final Map<FilePath, File> allFiles = new HashMap<>();
	
	/** A {@link Map} of all public variables and constants in the global scope */
	public static final Map<DataPath, String> globalVars = new HashMap<>();
	
	/** The File Main.pc, that contains the {@link MainFunction}. */
	private static File mainFile;
	
	static {
		try {
			mainFile = getFile(new FilePath(SRC + "..Main"));
		} catch (IOException e) {
			throw new InitException("Main.pc should be known by now.", e);
		}
		mainFile.findUsedDefs(new CallInfo(null, mainFile.path, MAIN.toString(), 0));
	}
	
	/**
	 * This returns all {@link File}s that are used in the importing-tree.
	 *
	 * @param path is the path of the imported {@link File}.
	 * @return the File at the path.
	 */
	public static File getFile(FilePath path) {
		File f = allFiles.get(path);
		if (f == null) {
			f = new File(path);
			allFiles.putIfAbsent(path, f);
		}
		return f;
	}
	
	/**
	 * Returns all lines of all imported Files merged into one {@link List}.
	 */
	public static List<Tuple<DataPath, String>> getLines() {
		List<Tuple<DataPath, String>> lines = new ArrayList<>(1000);
		for (File f : allFiles.values()) {
			lines.addAll(f.getRelevantContent());
		}
		return lines;
	}
}
