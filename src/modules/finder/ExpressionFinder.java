package modules.finder;

import expressions.normal.BuilderExpression;
import types.AbstractType;

public final class ExpressionFinder {

	/**
	 * Find the matching Expression out of the input and what is expected.
	 *
	 * @param current  is the currently read string.
	 * @param expected is an array of expected expressions.
	 * @return the matching Expression
	 * @throws IllegalArgumentException if no matching expression was found.
	 */
	public static BuilderExpression find(String current, int lineID, AbstractType... expected) {
		for (AbstractType expT : expected) {
			BuilderExpression exp = expT.create(current, lineID);
			if (exp != null)
				return exp;
		}
		return null;
	}
}
