package expressions.possible;

import static parsing.program.ExpressionType.*;

import datatypes.Value;
import expressions.normal.Expression;
import expressions.special.MergedExpression;
import expressions.special.ValueChanger;
import expressions.special.ValueHolder;
import interpreter.VarManager;

public class Assignment extends PossibleMainExpression implements ValueHolder, MergedExpression {

	private ValueChanger target;
	private ValueHolder value;

	public Assignment(int line) {
		super(line);
		setExpectedExpressions(NAME, OPEN_BRACKET, LITERAL, ARRAY_START);
	}

	/** Modifies the Value in the {@link VarManager} and returns the result. */
	@Override
	public Value getValue() {
		Value newVal = value.getValue();
		target.setValue(newVal);
		return newVal;
	}

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
