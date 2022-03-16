package misc.helper;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.stream.Stream;

import interpreting.modules.parser.Parser;

/**
 * Abstract Helperclass that contains all functions that get used all over the project.
 */
public abstract class Helper {

	/**
	 * Tells, if a char at a specified index is in an executable area (outside of string literals or
	 * comments).
	 * 
	 * @return true if the index is runnable
	 */
	public static boolean isRunnableCode(int index, String line) {
		return isNotInComment(index, line) && isNotInString(index, line);
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
	 * Returns the number of digits in a {@link BigInteger}.
	 */
	public static int getDigitCount(BigInteger number) {
		double factor = Math.log(2) / Math.log(10);
		int digitCount = (int) (factor * number.bitLength() + 1);
		if (BigInteger.TEN.pow(digitCount - 1).compareTo(number) > 0)
			return digitCount - 1;
		return digitCount;
	}

	/** Merges two arrays of the same type.*/
	@SuppressWarnings("unchecked")
	public static <T> T[] merge(T[] arr1, T[] arr2) {
		return (T[]) Stream.concat(Arrays.stream(arr1), Arrays.stream(arr2)).toArray(Object[]::new);
	}
}
