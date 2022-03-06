package interpreting.modules.assembler;

import java.util.ArrayList;
import java.util.List;

import building.types.specific.BuilderType;
import interpreting.exceptions.IllegalCodeFormatException;
import interpreting.modules.formatter.Formatter;
import interpreting.modules.parser.Parser;
import interpreting.modules.parser.Parser.IdxLine;
import misc.helper.Helper;

/**
 * Does all the invisible but necessary formatting that is not done by the {@link Formatter}.
 */
public class Assembler {

	private static List<IdxLine> lines;

	//@formatter:off
	static final String

			/** The symbol of a close-scope. */
			OS = BuilderType.OPEN_SCOPE.toString(),
	
			/** The symbol of a close-scope. */
			CS = BuilderType.CLOSE_SCOPE.toString(),
			
			/** The symbol of a multi-close-scope */
			MCS = String.valueOf(Parser.MULTI_CLOSE_SCOPE),
			
			/** The symbol of a single-line-comment */
			SLC = String.valueOf(Parser.SINGLE_LINE_COMMENT);
	
	//@formatter:on

	public static List<IdxLine> assemble(List<IdxLine> program) {
		// Strip all
		lines = new ArrayList<>(program.stream().map(e -> new IdxLine(e.line().strip(), e.index())).toList());
		removeEmptyAndComments();
		splitCloseScopes();
		splitOneLiners();
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
	 * If a line contains something except a CloseScope, it gets split into two lines.
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
	 */
	private static void splitCloseScopes() {
		for (int i = 0; i < lines.size(); i++) {
			final String l = lines.get(i).line();
			final int index = lines.get(i).index();
			if (l.contains(CS) && l.length() > 1) {
				// Remove original
				lines.remove(i);
				// Split the line, but keep the } symbols.
				final String[] line = l.split("((?<=" + CS + ")|(?=" + CS + "))");
				// Write the segments back
				for (int seg = 0; seg < line.length; seg++)
					lines.add(i + seg, new IdxLine(line[seg].strip(), index));
				// Jump over the new lines
				i += line.length;
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
	private static void splitOneLiners() {
		for (int i = 0; i < lines.size(); i++) {
			String l = lines.get(i).line();
			final int index = lines.get(i).index();

			int lineBreak = l.indexOf(":");
			if (lineBreak != -1 && Helper.isRunnableCode(lineBreak, l)) {
				if (lineBreak == l.length() - 1)
					throw new IllegalCodeFormatException(lines.get(i).index(), "This one-line statement has to end with a semicolon.");
				// Replace Semikolon with ScopeBrackets
				if (l.endsWith(MCS)) // For Nested Loops/Statements
					l = l.substring(0, l.length() - 1);
				lines.add(i + 1, new IdxLine(CS, index));
				lines.add(i + 1, new IdxLine(l.substring(lineBreak + 2), index)); // Teil nach :
				lines.set(i, new IdxLine(l.substring(0, lineBreak) + " " + OS, index));
			}
		}
	}

}
