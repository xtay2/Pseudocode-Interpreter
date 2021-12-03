package expressions.special;

import parser.program.ExpressionType;

/**
 * Every little part of a program is an expression. This includes names, values,
 * brackets and keywords. Each expression has a matching expressiontype which is
 * primarily used to describe what follows after an expression.
 *
 * @see ExpressionType
 */
public abstract class Expression {

	/** The line in which this Expression is defined.*/
	public final int line;

	public Expression(int line) {
		this.line = line;
	}

	/**
	 * The load of possible following expressions. {@code null} corresponds to an
	 * expected linebreak.
	 */
	protected ExpressionType[] expected = null;

	protected boolean isMainExpression = false;

	/**
	 * Used in all extending constructors.
	 *
	 * @param exp is the load of possible following expressions.
	 */
	protected final void setExpectedExpressions(ExpressionType... exp) {
		expected = exp;
	}

	/**
	 * @return All possible expected expression-types after this one. {@code null}
	 *         if the only expexted thing is a linebreak.
	 */
	public final ExpressionType[] getExpectedExpressions() {
		return expected;
	}

	/**
	 * Tells, if this is a main Expression
	 */
	public boolean isMainExpression() {
		return isMainExpression;
	}

	@Override
	public String toString() {
		String name = this.getClass().getName();
		String[] nameParts = name.split("\\.");
		if (nameParts.length > 0)
			return nameParts[nameParts.length - 1];
		return name;
	}
}
