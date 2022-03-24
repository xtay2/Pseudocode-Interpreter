package misc.helper;

import java.math.BigInteger;
import java.util.Arrays;

import interpreting.modules.parser.Parser;

/**
 * Abstract Helperclass that contains all functions that get used all over the
 * project.
 */
public abstract class Helper {

	/**
	 * Tells, if a char at a specified index is in an executable area (in the bounds
	 * of the line and outside of string literals or comments).
	 * 
	 * @return true if the index is runnable
	 */
	public static boolean isRunnableCode(int index, String line) {
		return index >= 0 && index < line.length() && isNotInComment(index, line) && isNotInString(index, line);
	}

	/**
	 * Tells, if a char at a specified index is not in the string boundaries. ("")
	 * 
	 * @return true if the index is not in a string.
	 */
	public static boolean isNotInString(int index, String line) {
		boolean inString = false;
		for (int i = 0; i < index; i++) {
			if (inString && line.charAt(i) == '\\')
				i++;
			else if (line.charAt(i) == '"')
				inString = !inString;
		}
		return !inString && index != -1;
	}

	/**
	 * Tells, if a char at a specified index is not in a comment. #
	 * 
	 * @return true if the index is not in a comment.
	 */
	private static boolean isNotInComment(int index, String line) {
		for (int i = 0; i < index; i++) {
			if (line.charAt(i) == Parser.SINGLE_LINE_COMMENT && isNotInString(i, line))
				return false;
		}
		return true;
	}

	/**
	 * Returns the index of the matching bracket in the same line.
	 * 
	 * @param fstIdx          is the index of the first bracket.
	 * @param line            is the whole line.
	 * @param isFullyRunnable should be true if the line contains no literal strings
	 *                        or comments. Default: false
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
			case '<' -> '>';
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
	 * Removes the char at the specified index and returns the line with lenght -1.
	 */
	public static String removeCharAt(int idx, String line) {
		return line.substring(0, idx) + line.substring(idx + 1);
	}

	/**
	 * Returns the number of digits in a {@link BigInteger}.
	 */
	public static int getDigitCount(BigInteger number) {
		double factor = Math.log(2) / Math.log(10);
		int digitCount = (int) (factor * number.bitLength() + 1);
		if (BigInteger.TEN.pow(digitCount - 1).compareTo(number) > 0)
			return digitCount - 1;
		return digitCount;
	}

	/** Merges two arrays of the same type. */
	public static <T> T[] merge(T[] arr1, T[] arr2) {
		T[] merged = Arrays.copyOf(arr1, arr1.length + arr2.length);
		System.arraycopy(arr2, 0, merged, arr1.length, arr2.length);
		return merged;
	}
}
