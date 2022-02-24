package expressions.main.functions;

import java.util.ArrayList;
import java.util.List;

import exceptions.runtime.IllegalCallException;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.ExpectedType;
import expressions.normal.containers.Name;
import expressions.possible.Call;
import modules.interpreter.Interpreter;
import modules.interpreter.system.SystemFunctions;

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

	/** [NAME] */
	@Override
	public void merge(Expression... e) {
		name = (Name) e[0];
		for (int i = 1; i < e.length; i++)
			expectedTypes.add((ExpectedType) e[i]);
	}

	@Override
	public boolean execute(ValueHolder... params) {
		if (params.length != expectedParams())
			throw new IllegalCallException(getOriginalLine(), "Illegal amount of params. Expected " + expectedParams());
		returnVal = SystemFunctions.callSystemFunc(SystemFunctions.getSystemFunction(getName()), params);
		return true;
	}

	@Override
	public int expectedParams() {
		return expectedTypes.size();
	}
}
