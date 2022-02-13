package expressions.abstractions;

import expressions.special.BuilderExpression;
import main.Main;
import parsing.program.ExpressionType;
import parsing.program.KeywordType;
import parsing.program.ValueMerger;

/**
 * Every little part of a program is an expression. This includes names, values,
 * brackets and keywords. Each expression has a matching {@link ExpressionType}
 * which is primarily used to describe what follows after an expression.
 *
 * @see MainExpression
 * @see BuilderExpression
 * @see MergedExpression
 * @see ExpressionType
 */
public abstract class Expression {

	/**
	 * The load of possible following expressions. {@code null} corresponds to an
	 * expected linebreak.
	 */
	protected ExpressionType[] expected = null;

	/** The line in which this Expression is defined. */
	public final int lineIdentifier;

	/** The Type of this Expression (non-null) */
	public final ExpressionType myType;

	/** Optional KeywordType (can be null) */
	public KeywordType myKeyword = null;

	public Expression(int line, KeywordType myKeyword) {
		this(line, ExpressionType.KEYWORD);
		this.myKeyword = myKeyword;
	}

	public Expression(int line, ExpressionType myType) {
		this.lineIdentifier = line;
		this.myType = myType;
		if(myType == null)
			throw new AssertionError("ExpressionType cannot be null.");
	}

	/**
	 * @return All possible expected expression-types after this one. {@code null}
	 *         if the only expexted thing is a linebreak.
	 */
	public final ExpressionType[] getExpectedExpressions() {
		if (expected == null)
			throw new AssertionError("The constructor of " + this + " must specify expected Expressions().");
		return expected;
	}

	/**
	 * Returns the corresponding line, shown in the text-editor of the user.
	 */
	public final int getOriginalLine() {
		return Main.PROGRAM.getLine(lineIdentifier).lineIndex;
	}

	/**
	 * Used in all extending constructors.
	 *
	 * @param exp is the load of possible following expressions.
	 */
	protected final void setExpectedExpressions(ExpressionType... exp) {
		if (exp != null)
			throw new AssertionError(this + " has already defined its expected Expressions.");
		expected = exp;
	}

	/**
	 * Used mostly in the {@link ValueMerger} for any {@link BuilderExpression} to
	 * assure, that this Expression is of a certain {@link ExpressionType}, when
	 * instanceof is no option.
	 */
	public final boolean is(ExpressionType t) {
		return t == myType;
	}

	/**
	 * Used mostly in the {@link ValueMerger} for any {@link BuilderExpression} to
	 * assure, that this Expression is of a certain {@link KeywordType}, when
	 * instanceof is no option.
	 */
	public final boolean is(KeywordType t) {
		return myKeyword != null && t == myKeyword;
	}

	/**
	 * Returns false for Expressions. Gets Overridden in
	 * {@link MainExpression#isDefiniteMainExpression}
	 */
	public boolean isDefiniteMainExpression() {
		return false;
	}

	/**
	 * Always returns the classname when in debuggingmode. If not, identifiers can
	 * be returned instead.
	 */
	@Override
	public String toString() {
		return this.getClass().getSimpleName();
	}
}
