package expressions.main.functions;

import java.util.ArrayList;
import java.util.List;

import exceptions.parsing.IllegalCodeFormatException;
import exceptions.runtime.IllegalCallException;
import expressions.abstractions.Expression;
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

	private final List<ExpectedType> expectedTypes = new ArrayList<>();

	/** Extends the constructor from {@link Returnable}. */
	public NativeFunction(int lineID) {
		super(lineID);
	}

	/** [NAME] (TYPE) (TYPE)... */
	@Override
	public void merge(Expression... e) {
		name = (Name) e[0];
		for (int i = 1; i < e.length - 1; i++) {
			if (e[i].type instanceof ExpectedType t)
				expectedTypes.add(t);
			else
				throw new AssertionError("This Method takes only one Name and BuilderExpressions with ExpectedTypes. Got: " + e[i]);
		}
		// Expected ReturnValue
		returnType = e[e.length - 1] == null ? null : (ExpectedType) e[e.length - 1].type;
		initNative();
	}

	@Override
	public boolean execute(ValueHolder... params) {
		finalCheck(); // Has to come first
		// Call to System-Functions
		if (params.length != expectedParams())
			throw new IllegalCallException(getOriginalLine(), "Illegal amount of params. Expected " + expectedParams());
		returnVal = SystemFunctions.callSystemFunc(SystemFunctions.getSystemFunction(getNameString()), params);
		if (returnVal != null && returnType != null)
			returnVal = returnVal.as(returnType);
		else {
			if (returnVal == null && returnType != null)
				throw new IllegalCodeFormatException(getOriginalLine(),
						getNameString() + " has speciefied the return-type " + returnType + " but had no return-value.");
			if (returnVal != null && returnType == null)
				throw new IllegalCodeFormatException(getOriginalLine(), getNameString() + " has to specify a return-type.");
		}
		return true;
	}

	@Override
	public int expectedParams() {
		return expectedTypes.size();
	}
}
