package programreader.expressions.special;

import programreader.interpreter.Interpreter;

/**
 * An Expression that can be build and executed.
 */
public abstract class MainExpression extends Expression {

	public MainExpression(int line) {
		super(line);
		isMainExpression = true;
	}

	/**
	 * Builds the MainExpression based on the expressions around. Should specify if
	 * it can be an oneLineStatement.
	 * 
	 * @param args are the expressions in this line.
	 */
	public abstract void build(Expression... args);

	/**
	 * Executes this MainExpression
	 * 
	 * @param doExecuteNext speciefies, if the next line should get executed.
	 *                      {@code false} if one-line-statement.
	 * @param params        are the parameters this Expression takes.
	 * 
	 * @return boolean that tells if the next function should be executed or not. Is
	 *         nearly exclusivly used by the ReturnStatement.
	 */
	public abstract boolean execute(boolean doExecuteNext, ValueHolder... params);

	public boolean callNextLine(boolean doExecuteNext) {
		if (doExecuteNext)
			return Interpreter.execute(line + 1, true);
		return true;
	}
}
