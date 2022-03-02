package expressions.possible;

import datatypes.ArrayValue;
import datatypes.DefValue;
import datatypes.Value;
import expressions.abstractions.PossibleMainExpression;
import expressions.abstractions.interfaces.NameHolder;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.functions.Returnable;
import expressions.normal.containers.Name;
import expressions.possible.multicall.MultiCall;
import expressions.possible.multicall.MultiCallable;
import types.SuperType;
import types.specific.data.ArrayType;

public class Call extends PossibleMainExpression implements ValueHolder, MultiCallable, NameHolder {

	private final DefValue calledFunc;
	private final ValueHolder[] parameters;

	/**
	 * Creates a {@link Call}.
	 * 
	 * @param calledFunc shouldn't be null.
	 * @param parameters shouldn't be null.
	 */
	public Call(int lineID, DefValue calledFunc, ValueHolder... parameters) {
		super(lineID, SuperType.MERGED);
		this.calledFunc = calledFunc;
		this.parameters = parameters;
		if (calledFunc == null || parameters == null)
			throw new AssertionError("CalledFunc and Params cannot be null.");
	}

	@Override
	public boolean execute() {
		getValue();
		return callNextLine();
	}

	@Override
	public Value getValue() {
		if (parameters.length == 1 && parameters[0] instanceof MultiCall m)
			return m.getValue();
		return calledFunc.call(parameters);
	}

	@Override
	public Value executeFor(ValueHolder[] content) {
		Value[] returnArr = new Value[content.length];
		for (int i = 0; i < content.length; i++)
			returnArr[i] = calledFunc.call(content[i]);
		// If the calls had return-values, return them in an array.
		if (returnArr[0] != null)
			return new ArrayValue(ArrayType.VAR_ARRAY, returnArr);
		// If not, dont return anything.
		return null;
	}

	/** Returns the name of the called {@link Returnable}. */
	@Override
	public Name getName() {
		return calledFunc.getName();
	}
}