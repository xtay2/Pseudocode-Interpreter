package importing.filedata;

import static misc.helper.ProgramHelper.*;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import building.types.specific.KeywordType;
import formatter.basic.Formatter;
import importing.filedata.paths.FilePath;
import interpreting.exceptions.IllegalCodeFormatException;
import misc.supporting.FileManager;
import runtime.exceptions.DefNotFoundException;

public class File {

	private final FilePath path;

	private final List<String> content;

	/** Set that contains all {@link DefInfo}s of this class. */
	private final Set<DefInfo> allDefs = new HashSet<>();

	/** Set that contains all called {@link DefInfo}s of this class. */
	private final Set<DefInfo> usedDefs = new HashSet<>();

	/** Set that contains all {@link CallInfo}s in all called {@link DefInfo}s in this class. */
	private final Set<CallInfo> usedCalls = new HashSet<>();

	public File(FilePath path, String startDef) {
		this.path = path;
		content = FileManager.readFile(path);
		preload();
	}

	private void preload() {
		for (int i = 0; i < content.size(); i++) {
			if (containsRunnable(content.get(i), "\\b" + KeywordType.FUNC + "\\b"))
				i = buildDefFromLine(i);
		}
	}

	/**
	 * Creates a {@link DefInfo} from {@link #content} and adds it to {@link #allDefs}.
	 *
	 * @param lineIdx is the line in which the definition begins.
	 * @return the lineIdx after the definition.
	 */
	private int buildDefFromLine(int lineIdx) {
		String line = content.get(lineIdx);
		// Find name
		String defName = getFirstRunnable(line, "(?<=" + KeywordType.FUNC + "\\s)\\w+(?=\\()");
		// Find args
		String params = getFirstRunnable(line, "\\([\\w,\\?\\[\\]\\s]*\\)");
		if (defName == null || params == null)
			throw new IllegalCodeFormatException(lineIdx, "Illegal use of " + KeywordType.FUNC);
		int argCnt = params.length() == 2 ? 0 : runnableMatches(params, ",") + 1;
		// Find end
		int end = lineIdx;
		if (!containsRunnable(line, ";"))
			end = findMatchingBrack(content, lineIdx, indexOfRunnable(line, Formatter.OBR))[0];
		if (!allDefs.add(new DefInfo(path, defName, argCnt, lineIdx, end)))
			throw new IllegalCodeFormatException(lineIdx, "Multiple Definitions of " + defName + " in " + path);
		return end + 1;
	}

	/**
	 * Gets called by another {@link File}.
	 *
	 * <pre>
	 * This Method:
	 * -Tries to find matching {@link DefInfo}s for every {@link CallInfo}.
	 * -If all {@link DefInfo}s are found, this preloads every other {@link DefInfo} for every outgoing {@link CallInfo}.
	 * </pre>
	 *
	 * @param incoming are the incoming {@link CallInfo}s from other {@link File}s.
	 */
	public void findUsedDefs(CallInfo... incoming) {
		Set<DefInfo> newlyAddedDefs = new HashSet<>();
		for (CallInfo ci : incoming) {
			for (DefInfo di : allDefs) {
				if (di.matches(ci))
					newlyAddedDefs.add(di);
				else
					throw new DefNotFoundException(-1, "Tried to call non-existent definition " + ci.targetName() + " in " + path);
			}
		}
		if (usedDefs.addAll(newlyAddedDefs))
			findUsedCalls(newlyAddedDefs);
	}

	/**
	 * After one {@link File} that imports this one, found all the matching {@link DefInfo}s to its
	 * {@link CallInfo}s, all outgoing calls from these newly added {@link DefInfo}s.
	 */
	private void findUsedCalls(Set<DefInfo> newelyAddedDefs) {
		Set<CallInfo> newelyAddedCalls = new HashSet<>();
		for (DefInfo def : newelyAddedDefs)
			newelyAddedCalls.addAll(findCallsInDef(def));
		callAllFiles(newelyAddedCalls);
	}

	private void callAllFiles(Set<CallInfo> newelyAddedCalls) {
		newelyAddedCalls.stream().sorted();
	}

	/**
	 * Finds all
	 *
	 * @param def
	 * @return
	 */
	private List<CallInfo> findCallsInDef(DefInfo def) {
		// TODO Auto-generated method stub
		return null;
	}

}
