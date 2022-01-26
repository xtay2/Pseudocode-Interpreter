package expressions.main.statements;

import static helper.Output.print;
import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;

import expressions.main.MainExpression;
import expressions.main.functions.Function;
import expressions.normal.Expression;
import expressions.special.ValueHolder;

public class ReturnStatement extends MainExpression implements Statement {

	private Function myFunc = null;
	private ValueHolder val = null;

	public ReturnStatement(int line) {
		super(line);
		setExpectedExpressions(LITERAL, NAME, ARRAY_START);
	}

	/** [VALUE] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 1)
			throw new AssertionError("Merge on a return has to contain one value.");
		val = (ValueHolder) e[0];
	}

	@Override
	public boolean execute(ValueHolder... params) {
		print("Returning " + (val != null ? val : ""));
		if (val != null)
			myFunc.setReturnVal(val.getValue());
		/*
		 * Fordere alle Expressions in dieser Funktion auf, keine weiteren
		 * execute-Funtionen auszuführen.
		 */
		return false;
	}

	public void setMyFunc(Function func) {
		myFunc = func;
	}
}
