package expressions.possible;

import static helper.Output.print;

import java.util.Arrays;

import datatypes.Value;
import expressions.abstractions.Expression;
import expressions.abstractions.PossibleMainExpression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.containers.Name;
import expressions.possible.multicall.MultiCall;
import expressions.possible.multicall.MultiCallable;
import modules.interpreter.Interpreter;
import types.SuperType;

public class Call extends PossibleMainExpression implements ValueHolder, MergedExpression, MultiCallable {

	private Name calledFunc = null;
	private ValueHolder[] parameters = null;

	public Call(int line) {
		super(line, SuperType.MERGED);
	}

	@Override
	public boolean execute(ValueHolder... params) {
		print("Calling Function " + calledFunc.getName() + (parameters.length == 0 ? "" : " with " + Arrays.toString(parameters)));
		getValue();
		return callNextLine();
	}

	public String getFuncName() {
		return calledFunc.getName();
	}

	@Override
	public Value getValue() {
		if (parameters.length == 1 && parameters[0] instanceof MultiCall m)
			return m.getValue();
		if (parameters == null)
			return Interpreter.call(getFuncName());
		return Interpreter.call(getFuncName(), parameters);
	}

	/**
	 * Gets build from an Array, containing the Name and parameters.
	 * 
	 * [Name] [?Param]...
	 */
	@Override
	public void merge(Expression... e) {
		calledFunc = (Name) e[0];
		parameters = new ValueHolder[e.length - 1];
		if (e.length > 1)
			System.arraycopy(e, 1, parameters, 0, e.length - 1);
	}

	@Override
	public Value executeFor(ValueHolder[] content) {
		for (ValueHolder p : content)
			Interpreter.call(getFuncName(), p);
		return null;
	}
}