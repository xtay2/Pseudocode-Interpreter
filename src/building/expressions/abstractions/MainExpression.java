package building.expressions.abstractions;

import building.expressions.abstractions.interfaces.NameHolder;
import building.types.abstractions.AbstractType;
import building.types.abstractions.SpecificType;
import interpreting.modules.interpreter.Interpreter;
import interpreting.modules.merger.ExpressionMerger;
import interpreting.modules.merger.SuperMerger;
import interpreting.program.ProgramLine;

/**
 * An Expression that can be build and executed. There exists only one MainExpression per line!
 * 
 * @see Expression
 * @see PossibleMainExpression
 */
public abstract class MainExpression extends Expression {

	/** The {@link Scope} this {@link Expression} lies in */
	private Scope myScope;

	/**
	 * Copies the following Constructor:
	 * 
	 * {@link Expression#Expression(int, Scope, AbstractType)}.
	 */
	public MainExpression(int lineID, SpecificType myType) {
		super(lineID, myType);
	}

	/**
	 * Sets the {@link Scope} this {@link MainExpression} lies in. (Can only get called once)
	 * 
	 * Gets called by the {@link ExpressionMerger#merge(ProgramLine)}.
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
	 * Returns the {@link Scope} this {@link MainExpression} lies in. If {@code this} is a
	 * {@link ScopeHolder}, this Method {@link #getScope()} gets overriden and returns the held Scope
	 * instead.
	 * 
	 * Gets called by the {@link SuperMerger}
	 */
	@Override
	public Scope getScope() {
		// Check for PossibleMainExpressions
		if (myScope == null && (myScope = super.getScope()) == null)
			throw new AssertionError("The Scope of " + (this instanceof NameHolder n ? n.getNameString() : this) + " wasn't registered.");
		return myScope;
	}

	/**
	 * Calls the next line. Write this after every return in nearly every {@link MainExpression}.
	 */
	public boolean callNextLine() {
		return Interpreter.execute(lineIdentifier + 1);
	}

	/**
	 * Executes this MainExpression
	 * 
	 * @return boolean that tells if the next function should be executed or not. Is nearly exclusivly
	 *         used by the ReturnStatement.
	 */
	public abstract boolean execute();

	/**
	 * Returns true if this is a MainExpression. Returns false if this is a PossibleMainExpression.
	 */
	@Override
	public final boolean isDefiniteMainExpression() {
		return !(this instanceof PossibleMainExpression);
	}
}
