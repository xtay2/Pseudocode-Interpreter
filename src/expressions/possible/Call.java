package expressions.possible;

import static helper.Output.print;

import java.util.Arrays;

import datatypes.Value;
import expressions.normal.Expression;
import expressions.normal.Name;
import expressions.special.MergedExpression;
import expressions.special.ValueHolder;
import interpreter.Interpreter;

public class Call extends PossibleMainExpression implements ValueHolder, MergedExpression {

	private Name calledFunc = null;
	private ValueHolder[] parameters = null;

	public Call(int line) {
		super(line);
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
		if (parameters == null)
			return Interpreter.call(getFuncName());
		return Interpreter.call(getFuncName(), parameters);
	}

	/**
	 * Gets build from an Array, containing the Name and parameters.
	 */
	@Override
	public void merge(Expression... e) {
		calledFunc = (Name) e[0];
		parameters = new ValueHolder[e.length - 1];
		if (e.length > 1)
			System.arraycopy(e, 1, parameters, 0, e.length - 1);
	}
}