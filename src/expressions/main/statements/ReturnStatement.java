package expressions.main.statements;

import static types.specific.BuilderType.ARRAY_START;
import static types.specific.ExpressionType.LITERAL;
import static types.specific.ExpressionType.NAME;

import expressions.abstractions.Expression;
import expressions.abstractions.MainExpression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.functions.Function;
import types.specific.KeywordType;

public class ReturnStatement extends MainExpression implements Statement {

	private Function myFunc = null;
	private ValueHolder val = null;

	public ReturnStatement(int line) {
		super(line, KeywordType.RETURN, LITERAL, NAME, ARRAY_START);
	}

	/** (VALUE) */
	@Override
	public void merge(Expression... e) {
		if (e.length > 1)
			throw new AssertionError("Merge on a return can take one value at max.");
		val = (ValueHolder) e[0];
	}

	/** Set the return-value of the function, and well... return. */
	@Override
	public boolean execute(ValueHolder... params) {
		if (myFunc == null)
			throw new AssertionError("This return-value has to be connected to a function.");
		if (val != null)
			myFunc.setReturnVal(val.getValue());
		return false;
	}

	/** Connect this {@link ReturnStatement} to a {@link Function}. */
	public void initFunc(Function func) {
		if (myFunc != null)
			throw new AssertionError("The function was already initialised.");
		myFunc = func;
	}
}
