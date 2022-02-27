package expressions.possible;

import static helper.Output.print;

import java.util.Arrays;

import datatypes.ArrayValue;
import datatypes.Value;
import expressions.abstractions.Expression;
import expressions.abstractions.PossibleMainExpression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.NameHolder;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.functions.Returnable;
import expressions.normal.containers.Name;
import expressions.possible.multicall.MultiCall;
import expressions.possible.multicall.MultiCallable;
import modules.interpreter.Interpreter;
import types.SuperType;
import types.specific.data.ArrayType;

public class Call extends PossibleMainExpression implements ValueHolder, MergedExpression, MultiCallable, NameHolder {

	private Name calledFunc = null;
	private ValueHolder[] parameters = null;

	public Call(int lineID) {
		super(lineID, SuperType.MERGED);
	}

	@Override
	public boolean execute(ValueHolder... params) {
		print("Calling Function " + calledFunc.getName() + (parameters.length == 0 ? "" : " with " + Arrays.toString(parameters)));
		getValue();
		return callNextLine();
	}

	@Override
	public Value getValue() {
		if (parameters.length == 1 && parameters[0] instanceof MultiCall m)
			return m.getValue();
		else if (parameters == null)
			return Interpreter.call(getNameString());
		else
			return Interpreter.call(getNameString(), parameters);
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
		Value[] returnArr = new Value[content.length];
		for (int i = 0; i < content.length; i++)
			returnArr[i] = Interpreter.call(getNameString(), content[i]);
		// If the calls had return-values, return them in an array.
		if (returnArr[0] != null) {
			ArrayValue a = new ArrayValue(ArrayType.VAR_ARRAY);
			a.merge(returnArr);
			return a;
		}
		// If not, dont return anything.
		return null;
	}

	/** Returns the name of the called {@link Returnable}. */
	@Override
	public Name getName() {
		return calledFunc.getName();
	}
}