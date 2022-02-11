package helper;

import java.math.BigInteger;

import parsing.parser.Parser;

/**
 * Abstract Helperclass that contains all functions that get used all over the
 * project.
 */
public abstract class Helper {

	/**
	 * Tells, if a char at a specified index is in an executable area (outside of
	 * string literals or comments).
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

	private static final BigInteger LONG_MAX = BigInteger.valueOf(Long.MAX_VALUE);

	public static BigInteger gcd(BigInteger a, BigInteger b) {
		System.out.println("Calling gcd with " + a + ", " + b);
		if (a.compareTo(LONG_MAX) < 0 && b.compareTo(LONG_MAX) < 0)
			return BigInteger.valueOf(gcdLongInt(a.longValueExact(), b.longValueExact()));
		return gcdBigInt(a, b);
	}

	/**
	 * Calculates the greatest common devisor between two longs. (Euclid)
	 * 
	 * used in {@link Helper#gcd()}
	 */
	private static long gcdLongInt(long a, long b) {
		a = Math.abs(a);
		b = Math.abs(b);
		System.out.println("long) a: " + a + ", b: " + b);
		while (b != 0) {
	        long t = a;
	        a = b;
	        b = t % b;
	    }
		System.out.println("long) res: " + a + "\n");
		return a;
	}

	/**
	 * Calculates the greatest common devisor for two {@link BigInteger}. (Euclid)
	 * 
	 * used in {@link Helper#gcd()}
	 */
	private static BigInteger gcdBigInt(BigInteger a, BigInteger b) {
		return a.gcd(a);
	}
}
