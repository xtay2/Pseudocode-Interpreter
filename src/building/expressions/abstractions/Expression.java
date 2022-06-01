package building.expressions.abstractions;

import building.expressions.abstractions.interfaces.AbstractExpression;
import building.expressions.normal.BuilderExpression;
import building.types.abstractions.AbstractType;
import building.types.abstractions.SpecificType;
import building.types.specific.KeywordType;
import importing.filedata.paths.DataPath;
import interpreting.modules.merger.SuperMerger;
import interpreting.program.ProgramLine;
import launching.Main;

/**
 * Every little part of a program is an expression. This includes names, values, brackets and
 * keywords. Each expression has a matching {@link ExpressionType} which is primarily used to
 * describe what follows after an expression.
 *
 * @see MainExpression
 * @see BuilderExpression
 * @see ExpressionType
 */
public abstract class Expression implements AbstractExpression {

	/** The line in which this Expression is defined. */
	public final int lineIdentifier;

	/** The Type of this Expression. */
	public final SpecificType type;

	/**
	 * Builds an Expression.
	 *
	 * @param lineID is the identifier of the matching {@link ProgramLine}.
	 * @param scope is the {@link Scope} in which this Variable is defined.
	 * @param myType is any {@link AbstractType} that describes this expression the best. Use a
	 * {@link SpecificType}, if possible.
	 * @param expected is an array of expected types following after this Expression.
	 */
	public Expression(int lineID, SpecificType myType) {
		this.lineIdentifier = lineID;
		this.type = myType;
		assert myType != null : "Type cannot be null.";
	}

	/**
	 * Used mostly in the {@link SuperMerger} for any {@link BuilderExpression} to assure, that this
	 * Expression is of a certain {@link KeywordType}, when instanceof is no option.
	 */
	@Override
	public final boolean is(AbstractType type) {
		return this.type.is(type);
	}

	/**
	 * Returns the {@link Scope} this {@link MainExpression} lies in. If {@code this} is a
	 * {@link ScopeHolder}, this Method {@link #getScope()} gets overriden and returns the held Scope
	 * instead.
	 *
	 * Gets called by the {@link SuperMerger}
	 */
	public Scope getScope() {
		return Main.PROGRAM.getLine(lineIdentifier).getMainExpression().getScope();
	}

	/**
	 * Returns false for Expressions. Gets Overridden in {@link MainExpression#isDefiniteMainExpression}
	 */
	@Override
	public boolean isDefiniteMainExpression() {
		return false;
	}

	@Override
	public DataPath getDataPath() {
		return Main.PROGRAM.getLine(lineIdentifier).dataPath;
	}

	/**
	 * Always returns the classname when in debuggingmode. If not, identifiers can be returned instead.
	 */
	@Override
	public String toString() {
		return getClass().getSimpleName();
	}
}
