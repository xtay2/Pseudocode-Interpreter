package formatter.basic;

import static building.types.abstractions.SpecificType.equalsString;
import static building.types.specific.KeywordType.*;
import static java.lang.String.valueOf;
import static misc.helper.ProgramHelper.*;
import static runtime.datatypes.numerical.ConceptualNrValue.NAN;
import static runtime.datatypes.numerical.ConceptualNrValue.POS_INF;

import java.util.ArrayList;
import java.util.List;

import building.expressions.main.CloseBlock;
import building.expressions.main.statements.FlagSpace;
import building.expressions.normal.brackets.OpenBlock;
import building.types.specific.FlagType;
import building.types.specific.KeywordType;
import building.types.specific.operators.InfixOpType;
import misc.constants.GreekSymbol;

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

	/**
	 * Gets executed before everything else.
	 *
	 * (Mainly before {@link FormatterLvl2#format()})
	 */
	protected static void preFormatting() {
		//@formatter:off
		forEachLine((x, y) -> replaceOperators(x, y),
				    (x, y) -> replaceSymbols(x, y),
				    (x, y) -> correctNrConsts(x, y));
		//@formatter:on
	}

	/**
	 * A {@link LineFormatterFunc} that replaces simple writings or operators with their
	 * unicode-equivalent.
	 */
	static String replaceOperators(String line, boolean isFullyRunnable) {
		line = replaceAllIfRunnable(line, "!=", InfixOpType.NOT_EQUALS.toString(), isFullyRunnable);
		line = replaceAllIfRunnable(line, "<=", InfixOpType.LESS_EQ.toString(), isFullyRunnable);
		line = replaceAllIfRunnable(line, ">=", InfixOpType.GREATER_EQ.toString(), isFullyRunnable);
		return line;
	}

	/**
	 * A {@link LineFormatterFunc} that replaces all escaped text-versions with greek characters.
	 */
	static String replaceSymbols(String line, boolean isFullyRunnable) {
		for (int i = 0; i < line.length(); i++) {
			if (line.charAt(i) == '\\' && (isFullyRunnable || isRunnableCode(i, line))) {
				for (int wordEnd = i + 1; wordEnd < line.length(); wordEnd++) {
					char c = line.charAt(wordEnd);
					if (c == ' ' || c == '(' || c == ')' || c == '[' || c == ']' || wordEnd == line.length()) {
						String word = line.substring(i, wordEnd);
						Character greek = GreekSymbol.fromString(word);
						if (greek != null)
							line = line.substring(0, i) + greek + line.substring(wordEnd);
					}
				}
			}
		}
		return line;
	}

	/** A {@link LineFormatterFunc} that corrects the case of number-constants. */
	static String correctNrConsts(String line, boolean isFullyRunnable) {
		line = replaceAllIfRunnable(line, "(?i)\\b" + NAN + "\\b", NAN.txt, isFullyRunnable);
		line = replaceAllIfRunnable(line, "(?i)\\b" + POS_INF + "\\b", POS_INF.txt, isFullyRunnable);
		return line;
	}

	protected static void format(boolean isMain) {
		if (isMain)
			addMissingMain();
		moveImportsUp();
		formatOpenScopes();
		formatClosedScopes();
		correctSemicolons();
//		//@formatter:off
		forEachLine(
				(x, y) -> correctFuncs(x, y),
				(x, y) -> replaceElifs(x, y),
				(x, y) -> orderFlags(x, y)
				);
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
	 * Move everything behind a {@link OpenBlock}-Bracket into the next line and connect the bracket to
	 * the statement.
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
			if (isRunnableCode(idxOfOs, line)) {
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
						if (isRunnableCode(0, line)) {
							program.set(j, line + " " + OB);
							break;
						}
					}
				}
			}
		}
	}

	/**
	 * If something stands in front of a {@link CloseBlock} the, CB and everything behind it gets moved
	 * to the next line.
	 *
	 * <pre>
	 *
	 * }} else {...}
	 *
	 * becomes
	 *
	 * }
	 * } else {...}
	 * -----------------------------------
	 * };}
	 *
	 * becomes
	 *
	 * };
	 * }
	 * </pre>
	 */
	static void formatClosedScopes() {
		for (int i = 0; i < program.size(); i++) {
			String line = program.get(i);
			int idxOfCs = line.indexOf(CB);
			// Line contains cs
			if (isRunnableCode(idxOfCs, line)) {
				String lineWthtFstCS = line.substring(idxOfCs + 1).stripLeading();
				if (!line.startsWith(CB)) {
					program.add(i, line.substring(0, idxOfCs));
					program.set(i + 1, line.substring(idxOfCs));
				} else if (!lineWthtFstCS.isBlank()) {
					// Only break the line if the next word isn't allowed as a follower.
					if (!CloseBlock.allowedAfter().stream().anyMatch(e -> lineWthtFstCS.startsWith(e.toString()))) {
						program.set(i, CB);
						program.add(i + 1, lineWthtFstCS);
					} // If a SLS ends with an actual block, the block has to be followed by a semicolon.
					else if (lineWthtFstCS.startsWith(MCS) && lineWthtFstCS.length() > 1) {
						program.set(i, CB + MCS);
						program.add(i + 1, lineWthtFstCS.substring(1));
					}
				}
			}
		}
	}

	/**
	 * Adds missing semicolons and removes unnecessary ones.
	 *
	 * <pre>
	 * Lines where semicolons get added:
	 * if a: return true
	 *
	 * if b: repeat {
	 * 	...
	 * }
	 *
	 * All other semicolons get removed.
	 * </pre>
	 */
	static void correctSemicolons() {
		for (int i = 0; i < program.size(); i++) {
			final String line = program.get(i);
			if (containsRunnable(line, String.valueOf(OLS))) {
				if (lineEndsWith(line, OB)) {
					int idxOfMatch = findMatchingBrack(program, i, indexOfRunnable(line, OBR))[0];
					if (!lineEndsWith(program.get(idxOfMatch), MCS))
						program.set(idxOfMatch, appendRunnable(program.get(idxOfMatch), MCS));
				} else if (!lineEndsWith(line, MCS))
					program.set(i, appendRunnable(line, MCS));
			} else if (!containsRunnable(line, CBR))
				program.set(i, replaceAllIfRunnable(line, MCS, "", isFullyRunnable(line)));
		}
	}

	/**
	 * A {@link LineFormatterFunc} that removes a space between the name of a function and its params.
	 *
	 * <pre>
	 * func function   (int x) {
	 * func function(int x)
	 * </pre>
	 */
	static String correctFuncs(String line, boolean isFullyRunnable) {
		return replaceAllIfRunnable(line, "(?<=func\\s\\w+)\\s+(?=\\()", "", isFullyRunnable);
	}

	/**
	 * A {@link LineFormatterFunc} that replaces wrong spellings of {@link KeywordType#ELIF}.
	 *
	 * If level2 is active, this should get executed after
	 * {@link FormatterLvl2#miscPadding(String, boolean)}
	 */
	static String replaceElifs(String line, boolean isFullyRunnable) {
		return replaceAllIfRunnable(line, ELSE + "(:?)\\s" + IF, ELIF.toString(), isFullyRunnable);
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
