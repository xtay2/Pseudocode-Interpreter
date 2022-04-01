package building.expressions.main.statements;

import static building.types.specific.KeywordType.RETURN;
import static misc.helper.Output.print;

import building.expressions.abstractions.MainExpression;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.functions.Definition;
import building.expressions.main.functions.Function;
import interpreting.exceptions.IllegalCodeFormatException;
import runtime.datatypes.Value;

public class ReturnStatement extends MainExpression {

	private Definition myFunc = null;
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
		if (val != null) {
			Value r = val.getValue();
			print("Returning: " + r);
			myFunc.setValue(r);
		}
		return false;
	}

	/** Connect this {@link ReturnStatement} to a {@link Function}. */
	public void initFunc(Definition def) {
		if (myFunc != null)
			throw new AssertionError("The function was already initialised.");
		if (def instanceof Function)
			myFunc = def;
		else if (val != null) {

			throw new IllegalCodeFormatException(getOriginalLine(),
					"Only return-statements that don't return values can be used in a " + def);
		}
	}
}
