package expressions.main.functions;

import java.util.List;

import datatypes.Value;
import exceptions.parsing.IllegalCodeFormatException;
import exceptions.runtime.IllegalCallException;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.containers.Name;
import expressions.possible.Call;
import modules.interpreter.Interpreter;
import modules.interpreter.system.SystemFunctions;
import types.specific.data.ExpectedType;

/**
 * This is the class for a native Function-Declaration.
 * 
 * If a {@link NativeFunction} called, this happens through the {@link Call}-Class and
 * {@link Interpreter#call}.
 */
public class NativeFunction extends Returnable {

	private final List<ExpectedType> params;

	/**
	 * Constructs a {@link NativeFunction}.
	 * 
	 * @param name       shouldn't be null.
	 * @param params     shouldn't be null.
	 * @param returnType can be null.
	 */
	public NativeFunction(int lineID, Name name, List<ExpectedType> params, ExpectedType returnType) {
		super(lineID, name, null);
		if (params == null)
			throw new AssertionError("Params cannot be null.");
		this.params = params;
		this.returnType = returnType;
	}

	@Override
	public int expectedParams() {
		return params.size();
	}

	@Override
	public Value call(ValueHolder... params) {
		finalCheck(); // Has to come first
		// Call to System-Functions
		if (params.length != expectedParams())
			throw new IllegalCallException(getOriginalLine(), "Illegal amount of params. Expected " + expectedParams());
		returnVal = SystemFunctions.callSystemFunc(SystemFunctions.getSystemFunction(getNameString()), params);
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
