package misc.helper;

import static formatter.basic.Formatter.MCS;
import static formatter.basic.Formatter.OLS;
import static formatter.basic.Formatter.SLC;
import static misc.helper.StringHelper.pointUnderline;

import java.util.Collections;
import java.util.List;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import formatter.basic.Formatter;

public final class ProgramHelper {

	private ProgramHelper() {
		// Dead constructor
	}

	/**
	 * Tells, if a char at a specified index is in an executable area.
	 *
	 * @return true if the index is runnable and false if its either out of bounds, in a string or in a
	 * comment.
	 */
	public static boolean isRunnableCode(int index, String line) {
		return isNotInComment(index, line) && isNotInString(index, line);
	}

	/**
	 * Tells, if a char at a specified index is not in the string and char boundaries.
	 *
	 * <pre>
	 * The symbols ' and " themselves are considered "in a string". (Returns false)
	 * If the index is out of bounds for line, its also considered as "in a string",
	 * because that means "don't execute this".
	 * </pre>
	 *
	 * @return true if the index is not in a string.
	 */
	public static boolean isNotInString(int index, String line) {
		if (index < 0 && index >= line.length())
			return false;
		boolean inString = false;
		for (int i = 0; i < index; i++) {
			if (inString && line.charAt(i) == '\\')
				i++;
			else if (line.charAt(i) == '"')
				inString = !inString;
			else if (SLC.equals(String.valueOf(line.charAt(i))))
				break;
		}
		return !inString && isNotAChar(index, line);
	}

	/**
	 * Tells, if a char at a specified index is not directly enclosed in char quotes.
	 *
	 * <pre>
	 * The symbols ' itself is considered "in a char". (Returns false)
	 * If the index is out of bounds for line, its also considered as "in a char",
	 * because that means "don't execute this".
	 * </pre>
	 *
	 * @return true if the index is not in a string.
	 */
	public static boolean isNotAChar(int index, String line) {
		if (index < 0 || index >= line.length())
			return false;
		if (index - 1 >= 0 && index + 1 < line.length())
			return line.charAt(index) != '\'' && !line.substring(index - 1, index + 2).matches("'.'");
		return line.charAt(index) != '\'';
	}

	/**
	 * Tells, if a char at a specified index is not in a comment.
	 *
	 * <pre>
	 * The symbol # itself is considered "in a comment". (Returns false)
	 * If the index is out of bounds for line, its also considered as "in a comment",
	 * because that means "don't execute this".
	 * </pre>
	 *
	 * @return true if the index is not in a comment.
	 */
	private static boolean isNotInComment(int index, String line) {
		int idxOfSLC = indexOfSLC(line);
		return (idxOfSLC == -1 || index < idxOfSLC) && index >= 0 && index < line.length();
	}

	/**
	 * Replaces all matches of the regex in the line with the replacement, if they are runnable code.
	 *
	 * @param line is the input line that gets tested
	 * @param regex is the pattern
	 * @param replacement is the replacement of the matches
	 * @param isFullyRunnable if the line was tested as fully runnable, the
	 * {@link String#replaceAll(String, String)} method gets chosen instead.
	 * @return the formatted string
	 */
	public static String replaceAllIfRunnable(String line, String regex, String replacement, boolean isFullyRunnable) {
		if (isFullyRunnable)
			return line.replaceAll(regex, replacement);
		Matcher m = Pattern.compile(regex).matcher(line);
		final String unedited = line;
		// Filter out all matches that aren't runnable
		List<MatchResult> matches = m.results().filter(r -> isRunnableCode(r.start(), unedited)).collect(Collectors.toList());
		// Replace all matches, back to front
		Collections.reverse(matches);
		for (MatchResult match : matches)
			line = line.substring(0, match.start()) + replacement + line.substring(match.end());
		return line;
	}

	/**
	 * Works like line += suffix, but if the line ends with a {@link Formatter#SLC}, the last spaces get
	 * stripped as well.
	 *
	 * <pre>
	 * line: burg #Comment
	 * suffix: er
	 * result: burger #Comment
	 * </pre>
	 */
	public static String appendRunnable(String line, String suffix) {
		int idxOfEnd = indexOfSLC(line);
		if (idxOfEnd == -1)
			return line + suffix;
		return line.substring(0, idxOfEnd).stripTrailing() + suffix + " " + line.substring(idxOfEnd);
	}

	/** Returns the index of a single-line-comment, or -1 if there is none in this line. */
	public static int indexOfSLC(String line) {
		int idxOfEnd = -1;
		do {
			idxOfEnd = line.indexOf(SLC, idxOfEnd + 1);
		} while (!isNotInString(idxOfEnd, line) && idxOfEnd != -1);
		return idxOfEnd;
	}

	/**
	 * Finds the matching bracket in a program.
	 *
	 * @param program is a list of lines (Strings).
	 * @param startLine is the index of the line that contains the bracket that searches its partner.
	 * @param idxOfFstBrack is the index of the character of the bracket, that searches its partner.
	 * @return a tuple with the following structure [lineIdxOfTarget, charIdxOfTarget]
	 * @throws IllegalStateException if the program doesn't contain a matching bracket.
	 */
	public static int[] findMatchingBrack(List<String> program, int startLine, int idxOfFstBrack) throws IllegalStateException {
		int[] result = findMatchingOpened(program, startLine, idxOfFstBrack);
		return result == null ? findMatchingClosed(program, startLine, idxOfFstBrack) : result;
	}

	/**
	 * Finds the matching opened bracket in a program.
	 *
	 * @param program is a list of lines (Strings).
	 * @param startLine is the index of the line that contains the bracket that searches its partner.
	 * @param idxOfFstBrack is the index of the character of the closed bracket, ") ] }" that searches
	 * its partner. "( [ {"
	 * @return a tuple with the following structure [lineIdxOfTarget, charIdxOfTarget]
	 * @throws IllegalStateException if the program doesn't contain a matching bracket.
	 */
	private static int[] findMatchingOpened(List<String> program, int startLine, int idxOfFstBrack) {
		final char start = program.get(startLine).charAt(idxOfFstBrack);
		final char target = switch (start) {
			case ')' -> '(';
			case ']' -> '[';
			case '}' -> '{';
			default -> ' ';
		};
		if (target == ' ')
			return null;
		int brack = 0;
		for (int lineIdx = startLine; lineIdx >= 0; lineIdx--) {
			String line = program.get(lineIdx);
			boolean isFullyRunnable = Formatter.isFullyRunnable(line);
			for (int i = startLine == lineIdx ? idxOfFstBrack : line.length() - 1; i >= 0; i--) {
				if (line.charAt(i) == start && (isFullyRunnable || isRunnableCode(i, line)))
					brack++;
				else if (line.charAt(i) == target && (isFullyRunnable || isRunnableCode(i, line)) && --brack == 0)
					return new int[] { lineIdx, i };
			}
		}
		throw new IllegalStateException("No matching open bracket was found.\n" + pointUnderline(program.get(startLine), idxOfFstBrack));
	}

	/**
	 * Finds the matching closed bracket in a program.
	 *
	 * @param program is a list of lines (Strings).
	 * @param startLine is the index of the line that contains the bracket that searches its partner.
	 * @param idxOfFstBrack is the index of the character of the closed bracket, "( [ { :" that searches
	 * its partner. ") ] } ;"
	 * @return a tuple with the following structure [lineIdxOfTarget, charIdxOfTarget]
	 * @throws IllegalStateException if the program doesn't contain a matching bracket.
	 */
	private static int[] findMatchingClosed(List<String> program, int startLine, int idxOfFstBrack) {
		final char start = program.get(startLine).charAt(idxOfFstBrack);
		if (start == ':')
			return findMatchingMCS(program, startLine);
		final char target = switch (start) {
			case '(' -> ')';
			case '[' -> ']';
			case '{' -> '}';
			default -> ' ';
		};
		if (target == ' ')
			return null;
		int brack = 0;
		for (int lineIdx = startLine; lineIdx < program.size(); lineIdx++) {
			String line = program.get(lineIdx);
			boolean isFullyRunnable = Formatter.isFullyRunnable(line);
			for (int i = startLine == lineIdx ? idxOfFstBrack : 0; i < line.length(); i++) {
				if (line.charAt(i) == start && (isFullyRunnable || isRunnableCode(i, line)))
					brack++;
				else if (line.charAt(i) == target && (isFullyRunnable || isRunnableCode(i, line)) && --brack == 0)
					return new int[] { lineIdx, i };
			}
		}
		throw new IllegalStateException("No matching close bracket was found.\n" + pointUnderline(program.get(startLine), idxOfFstBrack));
	}

	/**
	 * Finds the matching multi-close-scope symbol for a open-scope.
	 *
	 * @param program is a list of lines (Strings).
	 * @param startLine is the line that contains the
	 * @return a tuple with the following structure [lineIdxOfTarget, charIdxOfTarget]
	 */
	private static int[] findMatchingMCS(List<String> program, int startLine) {
		int exp = 0;
		for (int i = startLine; i < program.size(); i++) {
			final String line = program.get(i);
			if (containsRunnable(line, String.valueOf(OLS)))
				exp++;
			if (lineEndsWith(line, MCS))
				exp--;
			if (exp == 0)
				return new int[] { i, indexOfRunnable(line, MCS) };
		}
		throw new IllegalStateException(
				"No matching multi-close-scope bracket was found for the following line:\n" + program.get(startLine));
	}

	/**
	 * Returns the index of the matching bracket in the same line.
	 *
	 * @param fstIdx is the index of the first bracket.
	 * @param line is the whole line.
	 * @param isFullyRunnable should be true if the line contains no literal strings or comments.
	 * Default: false
	 * @return the index of the matching bracket or -1 if none was found.
	 *
	 * @throws AssertionError if the first index doesn't point to a bracket.
	 */
	public static int findMatchingBrackInLine(int fstIdx, String line, boolean isFullyRunnable) {
		char opened = line.charAt(fstIdx);
		char closed = switch (line.charAt(fstIdx)) {
			case '(' -> ')';
			case '[' -> ']';
			case '{' -> '}';
			default -> throw new AssertionError("Expected a bracket at index " + fstIdx + " got : \"" + line.charAt(fstIdx)
					+ "\" instead:\n" + line + "\n" + " ".repeat(fstIdx) + "^");
		};
		int brack = 1;
		for (int i = fstIdx + 1; i < line.length(); i++) {
			if (line.charAt(i) == opened && (isFullyRunnable || isRunnableCode(i, line)))
				brack++;
			else if (line.charAt(i) == closed && (isFullyRunnable || isRunnableCode(i, line))) {
				if (--brack == 0)
					return i;
			}
		}
		return -1;
	}

	/**
	 * This function tells, if there is any match of the regex in the line, that is also runnable.
	 *
	 * @param line is the whole line.
	 * @param regex is the regular expression that gets matched.
	 * @return true if the line contains that runnable expression.
	 */
	public static boolean containsRunnable(String line, String regex) {
		return indexOfRunnable(line, regex) != -1;
	}

	/**
	 * Works like {@link String#endsWith(String)}, but if there is comment at the end of the line, it
	 * gets ignored.
	 *
	 * <pre>
	 * false for suffix "burger" in line:
	 * I like #burger
	 *
	 * true for suffix "like" in line:
	 * I like #burger
	 * </pre>
	 *
	 * @param line is the whole line.
	 * @param suffix is the part that the end of the line gets checked against.
	 */
	public static boolean lineEndsWith(String line, String suffix) {
		int idxOfSLC = indexOfSLC(line);
		if (idxOfSLC == -1)
			return line.endsWith(suffix);
		return line.substring(0, idxOfSLC).stripTrailing().endsWith(suffix);
	}

	/**
	 * This function returns the first index of a match of the regex, in the line, that is also
	 * runnable.
	 *
	 * @param line is the whole line.
	 * @param regex is the regular expression that gets matched.
	 * @return the index of the start of the match.
	 */
	public static int indexOfRunnable(String line, String regex) {
		//@formatter:off
		MatchResult match = Pattern.compile(regex).matcher(line).results()
				.filter(mRes -> isRunnableCode(mRes.start(), line))
				.findFirst().orElseGet(() -> null);
		//@formatter:on
		return match == null ? -1 : match.start();
	}

	/**
	 * This function returns the first runnable {@link String}-match of the regex, in the line.
	 *
	 * @param line is the whole line.
	 * @param regex is the regular expression that gets matched.
	 * @return the first match.
	 */
	public static String getFirstRunnable(String line, String regex) {
		//@formatter:off
		MatchResult match = Pattern.compile(regex).matcher(line).results()
				.filter(mRes -> isRunnableCode(mRes.start(), line))
				.findFirst().orElseGet(() -> null);
		//@formatter:on
		return match == null ? null : match.group();
	}

	/**
	 * This function returns number of runnable occurences of the regex, in the line.
	 *
	 * @param line is the whole line.
	 * @param regex is the regular expression that gets matched.
	 * @return the number of runnable matches.
	 */
	public static int runnableMatches(String line, String regex) {
		return (int) Pattern.compile(regex).matcher(line).results() //
				.filter(mRes -> isRunnableCode(mRes.start(), line)).count();
	}
}
