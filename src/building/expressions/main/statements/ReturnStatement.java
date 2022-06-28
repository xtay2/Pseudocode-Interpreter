package building.expressions.main.statements;

import static building.types.specific.KeywordType.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.main.functions.*;
import errorhandeling.*;
import runtime.datatypes.*;

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
			throw new PseudocodeException("InvalidReturn",
					"Only return-statements that don't return values can be used in \"" + def + "\".", getBlueprintPath());
		}
	}
}
