package formatter.basic;

import static building.types.abstractions.SpecificType.equalsString;
import static building.types.specific.KeywordType.*;
import static java.lang.String.valueOf;

import java.util.ArrayList;
import java.util.List;

import building.expressions.main.CloseBlock;
import building.expressions.main.statements.FlagSpace;
import building.expressions.normal.brackets.OpenBlock;
import building.types.specific.FlagType;
import building.types.specific.KeywordType;
import building.types.specific.operators.InfixOpType;
import misc.helper.Helper;

/**
 * Everything should get executed after {@link FormatterLvl2#format()}.
 * 
 * <pre>
 * For the whole file:
 * {@link #addMissingMain()} 
 * {@link #moveImportsUp()}
 * {@link #formatOpenScopes()}
 * {@link #formatClosedScopes()}
 * 
 * For each line:
 * {@link #correctSemicolons(String, boolean)}
 * {@link #replaceElifs(String, boolean)}
 * {@link #orderFlags(String, boolean)}
 * {@link #replaceOperators(String, boolean)}
 * </pre>
 * 
 * @see Formatter
 */
public final class FormatterLvl1 extends Formatter {

	protected static void format(boolean isMain) {
		if (isMain)
			addMissingMain();
		moveImportsUp();
		formatOpenScopes();
		formatClosedScopes();
//		//@formatter:off
		forEachLine(List.of(
				(x, y) -> correctSemicolons(x, y),
				(x, y) -> replaceElifs(x, y),
				(x, y) -> orderFlags(x, y),
				(x, y) -> replaceOperators(x, y)
				));
		//@formatter:on
	}

	/**
	 * Adds a main-function if there isn't one already present.
	 */
	static void addMissingMain() {
		final String main = MAIN.toString();
		if (!program.stream().anyMatch(l -> containsRunnable(l, main + OSR))) {
			program.add(0, main + " " + OB);
			program.add(1, "\t" + SLC + "Implement me!");
			program.add(2, CB);
			program.add(3, "");
		}
	}

	/**
	 * Move all import statements to the top of the file.
	 * 
	 * This should get executed after {@link #addMissingMain()}
	 */
	static void moveImportsUp() {
		List<String> imports = new ArrayList<>();
		for (int i = program.size() - 1; i >= 0; i--) {
			if (program.get(i).startsWith(IMPORT + " "))
				imports.add(program.remove(i));
		}
		program.addAll(0, imports);
	}

	/**
	 * Formats {@link OpenBlock} Brackets.
	 * 
	 * Move everything behind a {@link OpenBlock}-Bracket into the next line and
	 * connect the bracket to the statement.
	 * 
	 * <pre>
	 * if false 
	 * 
	 * {print("hi")
	 * 
	 * becomes
	 * 
	 * if false {
	 *     print("hi")
	 * </pre>
	 */
	static void formatOpenScopes() {
		for (int i = 0; i < program.size(); i++) {
			String line = program.get(i);
			int idxOfOs = line.indexOf(OB);
			if (Helper.isRunnableCode(idxOfOs, line)) {
				// If something is behind the open block, move it down.
				if (idxOfOs != line.length() - 1) {
					program.add(i + 1, program.get(i).substring(idxOfOs + 1, line.length()));
					program.set(i, program.get(i).substring(0, idxOfOs + 1));
				}
				// If a line starts with an open block...
				if (line.stripIndent().startsWith(OB)) {
					program.remove(i);
					// Go back and reconnect it to the statement
					for (int j = i - 1; j >= 0; j--) {
						line = program.get(j);
						if (Helper.isRunnableCode(0, line)) {
							program.set(j, line + " " + OB);
							break;
						}
					}
				}
			}
		}
	}

	/**
	 * If something stands in front of a {@link CloseBlock} the, CB and everything
	 * behind it gets moved to the next line.
	 * 
	 * <pre>
	 * 
	 * }} else {...}
	 * 
	 * becomes
	 * 
	 * }
	 * } else {...}
	 * 
	 * </pre>
	 */
	static void formatClosedScopes() {
		for (int i = 0; i < program.size(); i++) {
			String line = program.get(i);
			int idxOfCs = line.indexOf(CB);
			if (Helper.isRunnableCode(idxOfCs, line)) {
				final String lineWthtFstCS = line.substring(idxOfCs + 1).stripLeading();
				if (!line.startsWith(CB)) {
					program.add(i, line.substring(0, idxOfCs));
					program.set(i + 1, line.substring(idxOfCs));
				}
				// If line starts with multiple OS, possibly seperated by blanks.
				else if (lineWthtFstCS.startsWith(CB)) {
					program.set(i, CB);
					program.add(i + 1, lineWthtFstCS);
				}
			}
		}
	}

	/**
	 * A {@link LineFormatterFunc} that adds a semicolon behind each
	 * one-line-statement thats missing one, and removes the unnecessary ones.
	 */
	static String correctSemicolons(String line, boolean isFullyRunnable) {
		// If the line doesn't contain any OLS', remove all semicolons
		if (line.indexOf(OLS) == -1)
			return replaceAllIfRunnable(line, MCS, "", isFullyRunnable);
		// If the line contains a OLS', it should end with a semicolon
		else if (!line.endsWith(MCS))
			return line + MCS;
		return line;
	}

	/**
	 * A {@link LineFormatterFunc} that replaces wrong spellings of
	 * {@link KeywordType#ELIF}.
	 * 
	 * If level2 is active, this should get executed after
	 * {@link FormatterLvl2#miscPadding(String, boolean)}
	 */
	static String replaceElifs(String line, boolean isFullyRunnable) {
		return replaceAllIfRunnable(line, ELSE + "(:?)\\s" + IF, ELIF.toString(), isFullyRunnable);
	}

	/**
	 * A {@link LineFormatterFunc} that replaces simple writings or operators with
	 * their unicode-equivalent.
	 */
	static String replaceOperators(String line, boolean isFullyRunnable) {
		line = replaceAllIfRunnable(line, "!=", InfixOpType.NOT_EQUALS.toString(), isFullyRunnable);
		line = replaceAllIfRunnable(line, "<=", InfixOpType.LESS_EQ.toString(), isFullyRunnable);
		line = replaceAllIfRunnable(line, ">=", InfixOpType.GREATER_EQ.toString(), isFullyRunnable);
		return line;
	}

	/**
	 * A {@link LineFormatterFunc} that:
	 * 
	 * <pre>
	 * Sets the order of Flags. 
	 * Removes unnecessary flags.
	 * Removes unnecessary {@value #OLS} in a one-line {@link FlagSpace}.
	 * </pre>
	 */
	static String orderFlags(String line, boolean isFullyRunnable) {
		List<String> flags = new ArrayList<>();
		String withoutFlags = line;
		while (withoutFlags.contains(" ")) {
			String fst = withoutFlags.substring(0, withoutFlags.indexOf(' '));
			// If the last flag ends with a :, this is an one-line-flagspace.
			if (fst.indexOf(OLS) != -1) {
				fst = fst.replace(valueOf(OLS), "");
				withoutFlags = replaceAllIfRunnable(withoutFlags, OLS + "|" + MCS, "", isFullyRunnable);
			}
			// If the first word is a flag it gets moved to #flags
			if (equalsString(fst, FlagType.class)) {
				flags.add(fst);
				withoutFlags = withoutFlags.substring(fst.length()).stripLeading();
			} else
				break;

		}
		if (flags.isEmpty())
			return line;
		return FlagType.orderFlags(flags).stream().reduce("", (r, w) -> r + w + " ") + withoutFlags;
	}
}
