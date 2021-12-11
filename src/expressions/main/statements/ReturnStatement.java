package expressions.main.statements;

import static helper.Output.print;

import datatypes.Castable;
import expressions.main.functions.Function;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.ValueHolder;
import parser.program.ExpressionType;

public class ReturnStatement extends MainExpression implements ValueHolder {

	private ValueHolder val = null;
	private Function myFunc = null;

	public ReturnStatement(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME, ExpressionType.ARRAY_START);
	}

	/** The Returnvalue */
	@Override
	public Castable getValue() {
		return val.getValue();
	}

	@Override
	public void build(Expression... args) {
		if (args.length == 2)
			val = (ValueHolder) args[1];
		else if (args.length > 2)
			throw new IllegalArgumentException("A function can only return one value.");
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

}
