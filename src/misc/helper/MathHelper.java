package misc.helper;

import java.math.BigInteger;

import building.expressions.abstractions.interfaces.ValueHolder;
import errorhandeling.NonExpressionException;

public class MathHelper {

	private MathHelper() {
		// Dead constructor
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

	/**
	 * Turns a {@link ValueHolder} into a {@link Integer}. This should only get used in special cases
	 * because problems that can occur are:
	 *
	 * @throws NonExpressionException if val isn't castable to int.
	 * @throws ArithmeticException if val is too big for int.
	 */
	public static int valToInt(ValueHolder val) throws NonExpressionException, ArithmeticException {
		return val.asInt().raw().intValueExact();
	}
}
