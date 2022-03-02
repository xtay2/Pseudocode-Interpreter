package expressions.main.statements;

import static helper.Output.print;
import static types.specific.KeywordType.RETURN;

import datatypes.Value;
import expressions.abstractions.MainExpression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.functions.Function;

public class ReturnStatement extends MainExpression {

	private Function myFunc = null;
	private final ValueHolder val;

	/**
	 * Creates a {@link ReturnStatement}.
	 * 
	 * @param val can be null.
	 */
	public ReturnStatement(int lineID, ValueHolder val) {
		super(lineID, RETURN);
		this.val = val;
	}

	/** Set the return-value of the function, and well... return. */
	@Override
	public boolean execute() {
		if (myFunc == null)
			throw new AssertionError("This return-value has to be connected to a function.");
		if (val != null) {
			Value r = val.getValue();
			print("Returning: " + r);
			myFunc.setValue(r);
		}
		return false;
	}

	/** Connect this {@link ReturnStatement} to a {@link Function}. */
	public void initFunc(Function func) {
		if (myFunc != null)
			throw new AssertionError("The function was already initialised.");
		myFunc = func;
	}
}
