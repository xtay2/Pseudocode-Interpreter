package building.expressions.abstractions;

import static building.types.SuperType.EXPECTED_TYPE;
import static building.types.specific.data.DataType.INT;
import static building.types.specific.data.DataType.NUMBER;

import building.expressions.normal.BuilderExpression;
import building.types.AbstractType;
import building.types.SuperType;
import building.types.specific.ExpressionType;
import building.types.specific.KeywordType;
import building.types.specific.data.ExpectedType;
import interpreting.modules.merger.SuperMerger;
import interpreting.program.ProgramLine;
import misc.main.Main;

/**
 * Every little part of a program is an expression. This includes names, values, brackets and
 * keywords. Each expression has a matching {@link ExpressionType} which is primarily used to
 * describe what follows after an expression.
 *
 * @see MainExpression
 * @see BuilderExpression
 * @see ExpressionType
 */
public abstract class Expression {

	/** The line in which this Expression is defined. */
	public final Integer lineIdentifier;

	/** The Type of this Expression. */
	public final AbstractType type;

	/** Constructor for all Expressions that don't necessarily have a scope. */
	public Expression(AbstractType myType) {
		this.type = myType;
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
	public Expression(int lineID, AbstractType myType) {
		this.lineIdentifier = lineID;
		this.type = myType;
		if (myType == null)
			throw new AssertionError("Type cannot be null.");
	}

	/**
	 * Returns the corresponding line, shown in the text-editor of the user.
	 */
	public final int getOriginalLine() {
		return Main.PROGRAM.getLine(lineIdentifier).orgLine;
	}

	/**
	 * Used mostly in the {@link SuperMerger} for any {@link BuilderExpression} to assure, that this
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
	 * Returns the {@link Scope} this {@link MainExpression} lies in. If {@code this} is a
	 * {@link ScopeHolder}, this Method {@link #getScope()} gets overriden and returns the held Scope
	 * instead.
	 * 
	 * Gets called by the {@link SuperMerger}
	 */
	public Scope getScope() {
		if (lineIdentifier != null)
			return Main.PROGRAM.getLine(lineIdentifier).getMainExpression().getScope();
		throw new AssertionError("This " + this + " has no scope.");
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
