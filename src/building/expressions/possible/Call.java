package building.expressions.possible;

import static building.types.abstractions.SpecificType.*;

import java.util.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.main.blueprints.*;
import building.expressions.main.functions.*;
import building.expressions.normal.containers.name.*;
import building.expressions.possible.multicall.*;
import errorhandeling.*;
import runtime.datatypes.*;
import runtime.datatypes.array.*;
import runtime.defmanager.*;

public class Call extends PossibleMainExpression implements MultiCallableValueHolder, NameHolder {
	
	private final Name calledFunc;
	private final ValueHolder[] parameters;
	private final Blueprint target;
	
	/** If the call contains a multi-call in the parameters, it get extracted in the constructor. */
	private MultiCall multiCall = null;
	
	private int idxOfMultiCall = -1;
	
	/**
	 * Creates a {@link Call}.
	 *
	 * @param calledFunc shouldn't be null.
	 * @param parameters shouldn't be null.
	 */
	public Call(int lineID, Blueprint target, Name calledFunc, ValueHolder... parameters) {
		super(lineID, MERGED);
		if (calledFunc == null || parameters == null)
			throw new AssertionError("CalledFunc and Params cannot be null.");
		this.target = target;
		this.calledFunc = calledFunc;
		this.parameters = parameters;
		// Check if there is a multicall in the parameters
		for (int i = 0; i < parameters.length; i++) {
			if (parameters[i] instanceof MultiCall mc) {
				if (multiCall != null) {
					throw new PseudocodeException("InvalidParameters", //
							"Tried to call " + getNameString() + " with multiple multi-calls.", getBlueprintPath());
				}
				multiCall = mc;
				idxOfMultiCall = i;
			}
		}
	}
	
	@Override
	public boolean execute() {
		getValue();
		return callNextLine();
	}
	
	@Override
	public Value getValue() {
		if (multiCall != null)
			return executeFor(multiCall.content);
		return callTarget(parameters);
	}
	
	@Override
	public Value executeFor(ValueHolder[] content) {
		Value[] returnArr = new Value[content.length];
		ValueHolder[] paramCopy = Arrays.copyOf(parameters, parameters.length);
		for (int i = 0; i < content.length; i++) {
			paramCopy[idxOfMultiCall] = content[i];
			returnArr[i] = callTarget(paramCopy);
		}
		// If the calls had return-values, return them in an array.
		if (returnArr[0] != null)
			return ArrayValue.newInstance(returnArr);
		// If not, dont return anything.
		return null;
	}
	
	/** Finds target-{@link Definition}, calls it with the params and returns the return-values. */
	private Value callTarget(ValueHolder... params) {
		return DefManager.get(calledFunc.getNameString(), parameters.length, target).call(params);
	}
	
	/** Returns the name of the called {@link Definition}. */
	@Override
	public Name getName() { return calledFunc.getName(); }
}