package expressions.abstractions;

import interpreter.Interpreter;
import parsing.program.ExpressionType;
import parsing.program.KeywordType;

/**
 * An Expression that can be build and executed. There exists only one
 * MainExpression per line!
 * 
 * @see Expression
 * @see MergedExpression
 * @see PossibleMainExpression
 */
public abstract class MainExpression extends Expression {

	public MainExpression(int line, KeywordType myKeyword) {
		super(line, myKeyword);
	}

	public MainExpression(int line, ExpressionType myType) {
		super(line, myType);
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
