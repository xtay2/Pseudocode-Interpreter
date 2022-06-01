package formatter.basic;

import static misc.helper.ProgramHelper.isRunnableCode;

import errorhandeling.PseudocodeException;
import importing.filedata.paths.DataPath;
import misc.helper.StringHelper;

/**
 * This class checks, if the {@link Formatter#program} can get "safely" formatted. Formatting-errors
 * might still occur.
 */
public final class FormattingPreChecks extends Formatter {

	protected static void check() {
		checkForLonelyBrackets();
	}

	/**
	 * Finds any occurrence of a bracket that has no match.
	 *
	 * @throws IllegalCodeFormatException if there are not equally many opened, as closed brackets.
	 */
	static void checkForLonelyBrackets() {
		int simple = 0, square = 0, curly = 0;
		for (int i = 0; i < program.size(); i++) {
			final String line = program.get(i);
			final boolean isFullyRunnable = isFullyRunnable(line);
			for (int j = 0; j < line.length(); j++) {
				char c = line.charAt(j);
				if (isFullyRunnable || isRunnableCode(j, line)) {
					switch (c) {
						case '(' -> simple++;
						case '[' -> square++;
						case '{' -> curly++;

						case ')' -> simple--;
						case ']' -> square--;
						case '}' -> curly--;
					}
					if (simple < 0 || square < 0 || curly < 0)
						throw new PseudocodeException("BracketMatch", //
								StringHelper.pointUnderline("The following bracket has no match:\n" + line, j), //
								new DataPath(filePath, i) //
						);
				}
			}
		}
		if (simple != 0 || curly != 0 || square != 0) {
			//@formatter:off
			throw new PseudocodeException("BracketMatch",
					"There exists atleast one unclosed bracket."
					+ "\nUnclosed \"( )\" brackets: " + simple
					+ "\nUnclosed \"[ ]\" brackets: " + square
					+ "\nUnclosed \"{ }\" brackets: " + curly,
					filePath
					);
			//@formatter:on
		}
	}
}
