package expressions.abstractions;

import static types.SuperType.EXPECTED_TYPE;
import static types.specific.data.DataType.INT;
import static types.specific.data.DataType.NUMBER;

import expressions.abstractions.interfaces.MergedExpression;
import expressions.normal.BuilderExpression;
import main.Main;
import modules.parser.program.ProgramLine;
import modules.parser.program.ValueMerger;
import types.AbstractType;
import types.SuperType;
import types.specific.ExpressionType;
import types.specific.KeywordType;
import types.specific.data.ExpectedType;

/**
 * Every little part of a program is an expression. This includes names, values, brackets and
 * keywords. Each expression has a matching {@link ExpressionType} which is primarily used to
 * describe what follows after an expression.
 *
 * @see MainExpression
 * @see BuilderExpression
 * @see MergedExpression
 * @see ExpressionType
 */
public abstract class Expression {

	/**
	 * The load of possible following expressions. {@code null} corresponds to an expected linebreak.
	 */
	protected final AbstractType[] expected;

	/** The line in which this Expression is defined. */
	public final Integer lineIdentifier;

	/** The scope in which this Expression is defined. */
	private Scope myScope = null;

	/** The Type of this Expression. */
	public final AbstractType type;

	/** Constructor for all Expressions that don't necessarily have a scope. */
	public Expression(AbstractType myType, AbstractType... expected) {
		this.type = myType;
		this.expected = expected;
		this.lineIdentifier = null;
	}

	/**
	 * Builds an Expression.
	 * 
	 * @param lineID   is the identifier of the matching {@link ProgramLine}.
	 * @param scope    is the scope in which this Variable is defined.
	 * @param myType   is any {@link AbstractType} that describes this expression the best. Use a
	 *                 {@link SpecificType}, if possible.
	 * @param expected is an array of expected types following after this Expression.
	 */
	public Expression(int lineID, AbstractType myType, AbstractType... expected) {
		this.lineIdentifier = lineID;
		this.type = myType;
		this.expected = expected;
		if (myType == null)
			throw new AssertionError("Type cannot be null.");
	}

	/**
	 * Sets the {@link Scope} this {@link Expression} lies in. (Can only get called once)
	 * 
	 * Gets called by the {@link ValueMerger}.
	 */
	public final void setScope(Scope s) {
		// Only functions for Expressions that have a lineID
		if (lineIdentifier == null)
			return;
		if (s == null)
			throw new AssertionError(getOriginalLine() + ": Scope cannot be set to null.");
		if (myScope != null && myScope != s)
			throw new AssertionError((lineIdentifier == -1 ? "Imported" : getOriginalLine()) + ": Tried to set the Scope of " + toString()
					+ " to " + s.getScopeName() + " but it was already set to " + myScope.getScopeName());
		myScope = s;
	}

	/**
	 * Returns the {@link Scope} this {@link Expression} lies in. If {@code this} is a
	 * {@link ScopeHolder}, this Method {@link #getScope()} gets overriden and returns the held Scope
	 * instead.
	 * 
	 * Gets called by the {@link ValueMerger}
	 */
	public Scope getScope() {
		if (myScope == null)
			throw new AssertionError("The Scope of " + this + " wasn't registered.");
		return myScope;
	}

	/**
	 * Returns all possible expected expression-types after this one or {@code null} if the only
	 * expexted thing is a linebreak.
	 */
	public final AbstractType[] getExpectedExpressions() {
		if (expected == null)
			throw new AssertionError("The constructor of " + this + " must specify expected expressions.");
		return expected;
	}

	/**
	 * Returns the corresponding line, shown in the text-editor of the user.
	 */
	public final int getOriginalLine() {
		return Main.PROGRAM.getLine(lineIdentifier).orgLine;
	}

	/**
	 * Used mostly in the {@link ValueMerger} for any {@link BuilderExpression} to assure, that this
	 * Expression is of a certain {@link KeywordType}, when instanceof is no option.
	 */
	public final boolean is(AbstractType type) {
		// @formatter:off
		return type == this.type 
				|| (type instanceof SuperType s && this.type.is(s)) 
				|| (type == NUMBER && this.type == INT)
				|| (type == EXPECTED_TYPE && this.type instanceof ExpectedType);
		// @formatter:on
	}

	/**
	 * Returns false for Expressions. Gets Overridden in {@link MainExpression#isDefiniteMainExpression}
	 */
	public boolean isDefiniteMainExpression() {
		return false;
	}

	/**
	 * Always returns the classname when in debuggingmode. If not, identifiers can be returned instead.
	 */
	@Override
	public String toString() {
		return this.getClass().getSimpleName();
	}
}
