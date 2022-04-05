package building.expressions.main.functions;

import static building.types.specific.FlagType.FINAL;
import static runtime.natives.SystemFunctions.callSystemFunc;
import static runtime.natives.SystemFunctions.getSystemFunction;

import java.util.Arrays;
import java.util.List;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.containers.Name;
import building.expressions.possible.Call;
import building.types.specific.DataType;
import building.types.specific.FlagType;
import interpreting.exceptions.IllegalCodeFormatException;
import interpreting.modules.interpreter.Interpreter;
import runtime.datatypes.Value;
import runtime.defmanager.DefManager;
import runtime.exceptions.DeclarationException;
import runtime.exceptions.IllegalCallException;
import runtime.natives.SystemFunctions;

/**
 * This is the class for a native Function-Declaration.
 * 
 * If a {@link NativeFunction} called, this happens through the {@link Call}-Class and
 * {@link Interpreter#call}.
 */
public class NativeFunction extends Definition {

	private final List<DataType> params;

	/**
	 * Defines and registers a {@link NativeFunction}.
	 * 
	 * @param name       is the unique {@link Name} of this {@link Definition}. shouldn't be null
	 * @param params     shouldn't be null.
	 * @param returnType is the {@link DataType} of the return value. Should be null if this is a void.
	 * @param flags      are optional {@link FlagType}s.
	 */
	public NativeFunction(int lineID, Name name, List<DataType> params, DataType returnType) {
		super(lineID, name, null);
		if (params == null)
			throw new AssertionError("Params cannot be null.");
		this.params = params;
		this.returnType = returnType;
		if (!Arrays.stream(SystemFunctions.SYSTEM_FUNCTION.values())
				.anyMatch(f -> f.name.equals(name.getNameString()) && f.args == params.size())) {
			throw new DeclarationException(getOriginalLine(),
					"Internally, there is no native function \"" + name + "<" + params.size() + ">\".");
		}
	}

	@Override
	public int expectedParams() {
		return params.size();
	}

	@Override
	public Value call(ValueHolder... params) {
		if (hasFlag(FINAL))
			DefManager.finalize(this);
		// Call to System-Functions
		if (params.length != expectedParams())
			throw new IllegalCallException(getOriginalLine(), "Illegal amount of params. Expected " + expectedParams());
		returnVal = callSystemFunc(getSystemFunction(getNameString(), getOriginalLine()), params);
		// The return-value is now set.
		if (returnVal != null && returnType != null)
			returnVal = returnVal.as(returnType);
		else {
			if (returnVal == null && returnType != null)
				throw new IllegalCodeFormatException(getOriginalLine(),
						getNameString() + " has speciefied the return-type " + returnType + " but had no return-value.");
			if (returnVal != null && returnType == null)
				throw new IllegalCodeFormatException(getOriginalLine(), getNameString() + " has to specify a return-type.");
		}
		Value temp = returnVal;
		returnVal = null;
		return temp;
	}
}
