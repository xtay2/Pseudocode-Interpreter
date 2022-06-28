package interpreting.modules.assembler;

import static formatter.basic.Formatter.*;
import static misc.helper.ProgramHelper.*;
import static misc.helper.StringHelper.*;

import java.util.*;

import building.expressions.main.*;
import building.types.specific.*;
import formatter.basic.Formatter;
import importing.filedata.paths.*;
import misc.util.*;

/** Does all the invisible but necessary formatting that is not done by the {@link Formatter}. */
public class Assembler {
	
	private static List<Tuple<DataPath, String>> lines;
	
	public static List<Tuple<DataPath, String>> assemble(List<Tuple<DataPath, String>> program) {
		// Strip all
		lines = program;
		removeEmptyAndComments();
		padRangeOperators();
		splitCloseBlocks();
		splitFullOneLiners();
		splitPartialOneLiners();
		return lines;
	}
	
	/** Remove all lines without content and single line comments. */
	private static void removeEmptyAndComments() {
		for (int i = lines.size() - 1; i >= 0; i--) {
			String l = lines.get(i).val2.strip();
			// Remove empty or fully commented
			if (l.isBlank() || l.startsWith(SLC)) {
				lines.remove(i);
				continue;
			}
			// Remove partial comments
			l = lineWithoutSLC(l);
			lines.set(i, new Tuple<>(lines.get(i).val1, l));
		}
	}
	
	/**
	 * Adds padding around all {@link BuilderType#RANGE}-Operators, so that they get distinguished from
	 * dots in decimal numbers.
	 */
	private static void padRangeOperators() {
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i).val2;
			lines.set(i, new Tuple<>(lines.get(i).val1, replaceAllIfRunnable(line, "\\.\\.", " .. ", false)));
		}
	}
	
	/**
	 * If a line contains something except a {@link CloseBlock}, it gets split into two lines.
	 *
	 * <pre>
	 * if ... {
	 *     if ... {...}
	 * } else {
	 *
	 * becomes
	 *
	 * if ... {
	 *     if ... {
	 *         ...
	 * }
	 * }
	 * else {
	 * </pre>
	 *
	 * An exeption is the partial one-line-statement, that ends with "};", and doesn't get split.
	 */
	private static void splitCloseBlocks() {
		for (int i = 0; i < lines.size(); i++) {
			final String l = lines.get(i).val2;
			if (containsRunnable(l, CB) && l.length() > 1 && !(CB + MCS).equals(l)) {
				// Remove original
				lines.remove(i);
				// Split the line, but keep the } symbols.
				final String[] line = l.split("((?<=" + CB + ")|(?=" + CB + "))");
				// Write the segments back
				for (int seg = 0; seg < line.length; seg++)
					lines.add(i + seg, new Tuple<>(lines.get(i).val1, line[seg].strip()));
				// Jump over the new lines
				i += line.length - 1;
			}
		}
	}
	
	/**
	 * Splits up all one-line-statements and removes the semicolon.
	 *
	 * <pre>
	 * if true: print("Hi");
	 *
	 * becomes
	 *
	 * if true {
	 * print("Hi")
	 * }
	 * </pre>
	 */
	private static void splitFullOneLiners() {
		for (int i = 0; i < lines.size(); i++) {
			final DataPath path = lines.get(i).val1;
			final String line = lines.get(i).val2;
			int idxOfFstOLS = indexOfRunnable(line, String.valueOf(OLS));
			if (idxOfFstOLS != -1 && lineEndsWith(line, MCS)) {
				lines.set(i, new Tuple<>(path, line.substring(0, idxOfFstOLS) + " " + OB));
				// SUBLINE
				String subLine = line.substring(idxOfFstOLS + 2);
				if (!containsRunnable(subLine, String.valueOf(OLS)))
					subLine = removeCharAt(indexOfRunnable(subLine, MCS), subLine);
				lines.add(i + 1, new Tuple<>(path, subLine));
				// CLOSE-BLOCK
				lines.add(i + 2, new Tuple<>(path, CB));
			}
		}
	}
	
	/**
	 * Splits up all partial one-line-statements and removes the semicolon.
	 *
	 * <pre>
	 * for e in [1, 2, 3]: if e % 2 == 0 {
	 * print(e)
	 * };
	 *
	 * becomes
	 *
	 * for e in [1, 2, 3] {
	 * if e % 2 == 0 {
	 * print(e)
	 * }
	 * }
	 * </pre>
	 */
	private static void splitPartialOneLiners() {
		for (int i = 0; i < lines.size(); i++) {
			final DataPath path = lines.get(i).val1;
			final String line = lines.get(i).val2;
			int idxOfFstOLS = indexOfRunnable(line, String.valueOf(OLS));
			if (idxOfFstOLS != -1) {
				if (!lineEndsWith(line, OB))
					throw new AssertionError( // The error-msg exists in this method, because it gets called after #splitFullOneLiners
							"A line that contains a one-line-start, has to end with an open-block or a multi-close-scope."
									+ "\nThis should get handled by FormatterLvl1.\nLine was: " + line);
				// CLOSE-BLOCK
				int idxOfMSC = findMatchingBrack(lines.stream().map(e -> e.val2).toList(), i, idxOfFstOLS)[0];
				lines.add(idxOfMSC + 1, new Tuple<>(lines.get(idxOfMSC).val1, CB));
				lines.set(i, new Tuple<>(path, line.substring(0, idxOfFstOLS) + " " + OB));
				lines.add(i + 1, new Tuple<>(path, line.substring(idxOfFstOLS + 2)));
			} else if ((CB + MCS).equals(line))
				lines.set(i, new Tuple<>(path, CB));
		}
	}
}
