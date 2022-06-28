package building.expressions.abstractions;

import building.expressions.main.blueprints.*;
import building.expressions.main.functions.*;
import building.types.abstractions.*;
import interpreting.modules.interpreter.*;

/**
 * An Expression that can be build and executed. There exists only one MainExpression per line!
 *
 * @see Expression
 * @see PossibleMainExpression
 */
public abstract class MainExpression extends Expression {
	
	/**
	 * Copies the following Constructor:
	 *
	 * {@link Expression#Expression(int, Scope, AbstractType)}.
	 */
	public MainExpression(int lineID, SpecificType myType) {
		super(lineID, myType);
	}
	
	/** Calls the next line. Write this after every return in nearly every {@link MainExpression}. */
	public boolean callNextLine() {
		return Interpreter.execute(lineIdentifier + 1);
	}
	
	/**
	 * Executes this {@link MainExpression}
	 *
	 * @return boolean that tells if the next function should be executed or not. Is nearly exclusivly
	 * used by the ReturnStatement.
	 *
	 * @throws AssertionError if this supports no direct execution. (See
	 * {@link Definition}/{@link Blueprint})
	 */
	public abstract boolean execute();
	
	/** Returns true if this is a MainExpression. Returns false if this is a PossibleMainExpression. */
	@Override
	public final boolean isDefiniteMainExpression() { return !(this instanceof PossibleMainExpression); }
}
