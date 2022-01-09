package expressions.main.statements;

import static helper.Output.print;

import datatypes.Value;
import expressions.main.functions.Function;
import expressions.normal.Semikolon;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.ValueHolder;
import helper.Output;
import parsing.program.ExpressionType;

public class ReturnStatement extends MainExpression implements ValueHolder {

	private ValueHolder val = null;
	private Function myFunc = null;

	public ReturnStatement(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME, ExpressionType.ARRAY_START, ExpressionType.DEFINITE_LINEBREAK);
	}

	/** The Returnvalue */
	@Override
	public Value getValue() {
		return val.getValue();
	}

	@Override
	public void build(Expression... args) {
		if (args.length == 1 || (args.length == 2 && args[1] instanceof Semikolon))
			return;
		if (args.length == 2 || (args.length == 3 && args[2] instanceof Semikolon))
			val = (ValueHolder) args[1];
		else if (args.length > 2)
			throw new AssertionError("A function can only return one value.");
	}

	public void setMyFunc(Function func) {
		myFunc = func;
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Returning " + (val != null ? val : ""));
		if(val != null)
			myFunc.setReturnVal(val.getValue());
		return false; //Fordere alle Expressions in dieser Funktion auf, keine weiteren execute-Funtionen auszuführen.
	}
	
	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "return";
	}
}
