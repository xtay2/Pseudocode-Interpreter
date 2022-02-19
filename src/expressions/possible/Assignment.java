package expressions.possible;

import static types.specific.BuilderType.ARRAY_START;
import static types.specific.BuilderType.OPEN_BRACKET;
import static types.specific.ExpressionType.LITERAL;
import static types.specific.ExpressionType.NAME;

import datatypes.Value;
import expressions.abstractions.Expression;
import expressions.abstractions.PossibleMainExpression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import modules.interpreter.VarManager;
import types.specific.ExpressionType;

public class Assignment extends PossibleMainExpression implements ValueHolder, MergedExpression {

	private ValueChanger target;
	private ValueHolder value;

	public Assignment(int line) {
		super(line, ExpressionType.ASSIGNMENT, LITERAL, NAME, OPEN_BRACKET, ARRAY_START);
	}

	/** Modifies the Value in the {@link VarManager} and returns the result. */
	@Override
	public Value getValue() {
		Value newVal = value.getValue();
		target.setValue(newVal);
		return newVal;
	}

	/** [ValueChanger] [ValueHolder] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 2)
			throw new AssertionError("");
		target = (ValueChanger) e[0];
		value = (ValueHolder) e[1];
	}

	@Override
	public boolean execute(ValueHolder... params) {
		getValue();
		return callNextLine();
	}
}
