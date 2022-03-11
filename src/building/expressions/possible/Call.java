package building.expressions.possible;

import building.expressions.abstractions.PossibleMainExpression;
import building.expressions.abstractions.interfaces.NameHolder;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.functions.Definition;
import building.expressions.normal.containers.Name;
import building.expressions.possible.multicall.MultiCall;
import building.expressions.possible.multicall.MultiCallable;
import building.types.SuperType;
import building.types.specific.data.ArrayType;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;
import runtime.datatypes.functional.DefLink;
import runtime.defmanager.DefManager;

public class Call extends PossibleMainExpression implements ValueHolder, MultiCallable, NameHolder {

	private final Name calledFunc;
	private final ValueHolder[] parameters;

	/**
	 * Creates a {@link Call}.
	 * 
	 * @param calledFunc shouldn't be null.
	 * @param parameters shouldn't be null.
	 */
	public Call(int lineID, Name calledFunc, ValueHolder... parameters) {
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
		return findTarget().call(parameters);
	}

	@Override
	public Value executeFor(ValueHolder[] content) {
		Value[] returnArr = new Value[content.length];
		for (int i = 0; i < content.length; i++)
			returnArr[i] = findTarget().call(content[i]);
		// If the calls had return-values, return them in an array.
		if (returnArr[0] != null)
			return new ArrayValue(ArrayType.VAR_ARRAY, returnArr);
		// If not, dont return anything.
		return null;
	}

	private Definition findTarget() {
		if (getScope().contains(calledFunc.getNameString()))
			return ((DefLink) getScope().getVar(calledFunc.getNameString(), getOriginalLine()).getValue()).raw();
		return DefManager.get(calledFunc.getNameString(), parameters.length, getOriginalLine());
	}

	/** Returns the name of the called {@link Definition}. */
	@Override
	public Name getName() {
		return calledFunc.getName();
	}
}