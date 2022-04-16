package interpreting.modules.assembler;

import static formatter.basic.Formatter.*;
import static misc.helper.ProgramHelper.containsRunnable;
import static misc.helper.ProgramHelper.indexOfRunnable;
import static misc.helper.ProgramHelper.lineEndsWith;
import static misc.helper.StringHelper.removeCharAt;
import static misc.helper.StringHelper.unIndexLines;

import java.util.ArrayList;
import java.util.List;

import building.expressions.main.CloseBlock;
import building.types.specific.BuilderType;
import formatter.basic.Formatter;
import interpreting.modules.parser.Parser.IdxLine;
import misc.helper.ProgramHelper;

/**
 * Does all the invisible but necessary formatting that is not done by the {@link Formatter}.
 */
public class Assembler {

	private static List<IdxLine> lines;

	public static List<IdxLine> assemble(List<IdxLine> program) {
		// Strip all
		lines = new ArrayList<>(program.stream().map(e -> new IdxLine(e.line().strip(), e.index())).toList());
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
			final String l = lines.get(i).line();
			final int index = lines.get(i).index();
			// Remove empty or fully commented
			if (l.isBlank() || l.startsWith(SLC))
				lines.remove(i);
			// Remove partial comments
			else if (l.contains(SLC))
				lines.set(i, new IdxLine(l.substring(0, l.indexOf(SLC)).stripTrailing(), index));
		}
	}

	/**
	 * Adds padding around all {@link BuilderType#RANGE}-Operators, so that they get distinguished from
	 * dots in decimal numbers.
	 */
	private static void padRangeOperators() {
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i).line();
			lines.set(i, new IdxLine(ProgramHelper.replaceAllIfRunnable(line, "\\.\\.", " .. ", false), i));
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
			final String l = lines.get(i).line();
			final int index = lines.get(i).index();
			if (containsRunnable(l, CB) && l.length() > 1 && !l.equals(CB + MCS)) {
				// Remove original
				lines.remove(i);
				// Split the line, but keep the } symbols.
				final String[] line = l.split("((?<=" + CB + ")|(?=" + CB + "))");
				// Write the segments back
				for (int seg = 0; seg < line.length; seg++)
					lines.add(i + seg, new IdxLine(line[seg].strip(), index));
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
			final String line = lines.get(i).line();
			final int orgLine = lines.get(i).index();
			int idxOfFstOLS = indexOfRunnable(line, String.valueOf(OLS));
			if (idxOfFstOLS != -1) {
				if (lineEndsWith(line, MCS)) {
					lines.set(i, new IdxLine(line.substring(0, idxOfFstOLS) + " " + OB, orgLine));
					// SUBLINE
					String subLine = line.substring(idxOfFstOLS + 2);
					if (!containsRunnable(subLine, String.valueOf(OLS)))
						subLine = removeCharAt(indexOfRunnable(subLine, MCS), subLine);
					lines.add(i + 1, new IdxLine(subLine, orgLine));
					// CLOSE-BLOCK
					lines.add(i + 2, new IdxLine(CB, orgLine));
				}
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
			final String line = lines.get(i).line();
			final int orgLine = lines.get(i).index();
			int idxOfFstOLS = indexOfRunnable(line, String.valueOf(OLS));
			if (idxOfFstOLS != -1) {
				if (lineEndsWith(line, OB)) {
					// CLOSE-BLOCK
					int idxOfMSC = ProgramHelper.findMatchingBrack(unIndexLines(lines), i, idxOfFstOLS)[0];
					lines.add(idxOfMSC + 1, new IdxLine(CB, lines.get(idxOfMSC).index()));
					lines.set(i, new IdxLine(line.substring(0, idxOfFstOLS) + " " + OB, orgLine));
					lines.add(i + 1, new IdxLine(line.substring(idxOfFstOLS + 2), orgLine));
				} else {
					//@formatter:off
					throw new AssertionError( // The error-msg exists in this method, because it gets called after #splitFullOneLiners
							"A line that contains a one-line-start, has to end with an open-block or a multi-close-scope."
							+ "\nThis should get handled by FormatterLvl1.\nLine was: " + line);
				} //@formatter:on
			} else if (line.equals(CB + MCS))
				lines.set(i, new IdxLine(CB, orgLine));
		}
	}
}
