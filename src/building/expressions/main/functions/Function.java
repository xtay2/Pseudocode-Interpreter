package building.expressions.main.functions;

import static building.types.specific.FlagType.*;

import java.util.*;
import java.util.Map.*;

import building.expressions.abstractions.interfaces.*;
import building.expressions.abstractions.scopes.*;
import building.expressions.normal.brackets.*;
import building.expressions.normal.containers.*;
import building.expressions.normal.containers.name.*;
import building.expressions.possible.*;
import building.types.specific.*;
import building.types.specific.datatypes.*;
import errorhandeling.*;
import interpreting.modules.interpreter.*;
import runtime.datatypes.*;
import runtime.defmanager.*;

/**
 * This is the class for a Function-Declaration.
 *
 * If a {@link Function} gets called, this happens through the {@link Call}-Class and
 * {@link Interpreter#call}.
 */
public class Function extends Definition implements NameHolder {
	
	/** All expected parameters. */
	private final LinkedHashMap<Name, DataType> paramBlueprint;
	
	/**
	 * Defines and registers a {@link Function}.
	 *
	 * @param name is the unique {@link Name} of this {@link Definition}.
	 * @param params is the {@link LinkedHashMap} of all parameters (types and names) of this
	 * {@link Function}. Shouldn't be null.
	 * @param returnType is the {@link DataType} of the return value. Should be null if this is a void.
	 * @param os is the {@link OpenBlock} of this {@link ScopeHolder}. Shouldn't be null.
	 * @param flags are optional {@link FlagType}s.
	 */
	public Function(int lineID, VarName name, LinkedHashMap<Name, DataType> params, DataType returnType, OpenBlock os) {
		super(lineID, name, returnType, os);
		if (params == null)
			throw new AssertionError("Params cannot be null.");
		if (name.getNameString().equals(KeywordType.MAIN.toString()))
			throw new PseudocodeException("IllegalFuncName", "An ordinary function cannot be called main.", getBlueprintPath());
		this.paramBlueprint = params;
	}
	
	@Override
	public Value call(ValueHolder... params) {
		if (expectedParams() != params.length) {
			throw new PseudocodeException("WrongParamCount",
					"The function " + getNameString() + " is called with the wrong amount of params. " //
							+ "\nWere: " + params.length + " but should have been " + expectedParams() + ".",
					getBlueprintPath());
		}
		// Check if this was already called.
		if (hasFlag(FINAL))
			DefManager.finalize(this);
		// Init Params
		int i = 0;
		for (Entry<Name, DataType> param : paramBlueprint.entrySet()) {
			Value v = params[i++].getValue();
			new Variable(lineIdentifier, param.getValue(), param.getKey(), v);
		}
		callFirstLine();
		// The return-value is now set.
		if (returnType != null && returnVal == null) {
			throw new PseudocodeException("IllegalReturn", //
					getNameString() + " was defined to return a value of type: " + returnType + ", but returned nothing.", //
					getBlueprintPath());
		}
		Value temp = returnVal;
		returnVal = null;
		return temp;
	}
	
	@Override
	public int expectedParams() {
		return paramBlueprint.size();
	}
}
