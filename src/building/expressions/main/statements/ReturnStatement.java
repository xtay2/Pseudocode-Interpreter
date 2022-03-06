package building.expressions.main.statements;

import static building.types.specific.KeywordType.RETURN;
import static misc.helper.Output.print;

import building.expressions.abstractions.MainExpression;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.functions.Function;
import runtime.datatypes.Value;

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
