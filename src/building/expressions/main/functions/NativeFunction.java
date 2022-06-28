package building.expressions.main.functions;

import static building.types.specific.FlagType.*;
import static runtime.natives.SystemFunctions.*;

import java.util.*;

import building.expressions.abstractions.interfaces.*;
import building.expressions.normal.containers.name.*;
import building.expressions.possible.*;
import building.types.specific.*;
import building.types.specific.datatypes.*;
import errorhandeling.*;
import interpreting.modules.interpreter.*;
import runtime.datatypes.*;
import runtime.defmanager.*;
import runtime.natives.*;

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
	 * @param name is the unique {@link Name} of this {@link Definition}. shouldn't be null
	 * @param params shouldn't be null.
	 * @param returnType is the {@link DataType} of the return value. Should be null if this is a void.
	 * @param flags are optional {@link FlagType}s.
	 */
	public NativeFunction(int lineID, VarName name, List<DataType> params, DataType returnType) {
		super(lineID, name, returnType, null);
		assert params != null : "Params cannot be null.";
		this.params = params;
		if (!Arrays.stream(SystemFunctions.SYSTEM_FUNCTION.values())
				.anyMatch(f -> f.name.equals(name.getNameString()) && f.argTypes.length == params.size())) {
			throw new PseudocodeException("NativeDeclaration",
					"Internally, there is no native function \"" + name + "<" + params.size() + ">\".", //
					getBlueprintPath());
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
			throw new PseudocodeException("IllegalCall", "Illegal amount of params. Expected " + expectedParams(), getBlueprintPath());
		try {
			returnVal = callSystemFunc(getSystemFunction(getName()), params);
			// The return-value is now set.
			if (returnVal != null && returnType != null)
				returnVal = returnVal.as(returnType);
			else {
				if (returnVal == null && returnType != null) {
					throw new PseudocodeException("MissingReturn", //
							getNameString() + " has speciefied the return-type " + returnType + " but had no return-value.", //
							getBlueprintPath());
				}
				if (returnVal != null && returnType == null) {
					throw new PseudocodeException("UnexpectedReturn", //
							"The function " + getNameString() + " expected a return-value of type " + returnType //
									+ " but returned the value " + returnVal + "instead.",
							getBlueprintPath());
				}
			}
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, getBlueprintPath());
		}
		Value temp = returnVal;
		returnVal = null;
		return temp;
	}
}
