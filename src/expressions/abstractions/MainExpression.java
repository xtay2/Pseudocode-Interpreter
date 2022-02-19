package expressions.abstractions;

import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueHolder;
import modules.interpreter.Interpreter;
import types.AbstractType;

/**
 * An Expression that can be build and executed. There exists only one
 * MainExpression per line!
 * 
 * @see Expression
 * @see MergedExpression
 * @see PossibleMainExpression
 */
public abstract class MainExpression extends Expression {

	/**
	 * Copies the following Constructor:
	 * {@link Expression#Expression(int, AbstractType, AbstractType...)}.
	 */
	public MainExpression(int lineID, AbstractType myType, AbstractType... expected) {
		super(lineID, myType, expected);
	}

	/**
	 * Calls the next line. Write this after every return in nearly every
	 * {@link MainExpression}.
	 */
	public boolean callNextLine() {
		return Interpreter.execute(lineIdentifier + 1);
	}

	/**
	 * Executes this MainExpression
	 *
	 * @param params are the parameters this Expression takes.
	 *
	 * @return boolean that tells if the next function should be executed or not. Is
	 *         nearly exclusivly used by the ReturnStatement.
	 */
	public abstract boolean execute(ValueHolder... params);

	/**
	 * Returns true if this is a MainExpression. Returns false if this is a
	 * PossibleMainExpression.
	 */
	@Override
	public final boolean isDefiniteMainExpression() {
		return !(this instanceof PossibleMainExpression);
	}
}
