package expressions.main.statements;

import static helper.Output.print;
import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.DEFINITE_LINEBREAK;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;

import datatypes.Value;
import expressions.main.functions.Function;
import expressions.normal.Semicolon;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.ValueHolder;
import helper.Output;

public class ReturnStatement extends MainExpression implements ValueHolder {

	private Function myFunc = null;
	private ValueHolder val = null;

	public ReturnStatement(int line) {
		super(line);
		setExpectedExpressions(LITERAL, NAME, ARRAY_START, DEFINITE_LINEBREAK);
	}

	@Override
	public void build(Expression... args) {
		if (args.length == 1 || (args.length == 2 && args[1] instanceof Semicolon))
			return;
		if (args.length == 2 || (args.length == 3 && args[2] instanceof Semicolon))
			val = (ValueHolder) args[1];
		else if (args.length > 2)
			throw new AssertionError("A function can only return one value.");
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Returning " + (val != null ? val : ""));
		if (val != null)
			myFunc.setReturnVal(val.getValue());
		return false; // Fordere alle Expressions in dieser Funktion auf, keine weiteren
						// execute-Funtionen auszuführen.
	}

	/** The Returnvalue */
	@Override
	public Value getValue() {
		return val.getValue();
	}

	public void setMyFunc(Function func) {
		myFunc = func;
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "return";
	}
}
