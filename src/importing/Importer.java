package importing;

import static building.types.specific.KeywordType.MAIN;
import static importing.filedata.paths.Location.SRC;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import building.expressions.main.functions.MainFunction;
import importing.filedata.CallInfo;
import importing.filedata.File;
import importing.filedata.paths.DataPath;
import importing.filedata.paths.FilePath;
import misc.util.Tuple;

/**
 * The class that is controlling all imports.
 *
 * <pre>
 * -All methods in this class should be static.
 * -This class mustn't have sublasses.
 * </pre>
 */
public abstract class Importer {

	private static Map<FilePath, File> allFiles = new HashMap<>();

	/** The File Main.pc, that contains the {@link MainFunction}. */
	private static File mainFile = getFile(new FilePath(SRC + "..Main"));

	static {
		try {
			// Start search with main in Main.pc
			mainFile.findUsedDefs(new CallInfo(mainFile.path, MAIN.toString(), 0));
		} catch (Exception e) {
			e.printStackTrace();
			throw new AssertionError("Couldn't detect main-function in Main.pc");
		}
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
		for (File f : allFiles.values())
			lines.addAll(f.getRelevantContent());
		return lines;
	}
}
