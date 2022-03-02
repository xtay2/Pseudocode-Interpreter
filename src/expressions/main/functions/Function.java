package expressions.main.functions;

import java.util.LinkedHashMap;
import java.util.Map.Entry;

import datatypes.Value;
import exceptions.runtime.IllegalCallException;
import exceptions.runtime.IllegalReturnException;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.brackets.OpenScope;
import expressions.normal.containers.Name;
import expressions.normal.containers.Variable;
import expressions.possible.Call;
import modules.interpreter.Interpreter;
import types.specific.data.ExpectedType;

/**
 * This is the class for a Function-Declaration.
 * 
 * If a {@link Function} gets called, this happens through the {@link Call}-Class and
 * {@link Interpreter#call}.
 */
public class Function extends Returnable {

	/** All expected parameters. */
	private final LinkedHashMap<Name, ExpectedType> paramBlueprint;

	/** Extends the constructor from {@link Returnable}. */
	public Function(int lineID, Name name, LinkedHashMap<Name, ExpectedType> params, ExpectedType returnType, OpenScope os) {
		super(lineID, name, os);
		if (params == null)
			throw new AssertionError("Params cannot be null.");
		this.paramBlueprint = params;
		this.returnType = returnType;
	}

	@Override
	public Value call(ValueHolder... params) {
		if (expectedParams() != params.length)
			throw new IllegalCallException(getOriginalLine(), "This function is called with the wrong amount of params.");
		finalCheck();
		// Init Params
		int i = 0;
		for (Entry<Name, ExpectedType> param : paramBlueprint.entrySet()) {
			Value v = params[i++].getValue();
			Variable.quickCreate(lineIdentifier, getScope(), param.getValue(), param.getKey(), v);
		}
		callFirstLine();
		getScope().clear();
		// The return-value is now set.
		if (returnType != null && returnVal == null) {
			throw new IllegalReturnException(getOriginalLine(),
					getNameString() + " was defined to return a value of type: " + returnType + ", but returned nothing.");
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
