package programreader.expressions.main.statements;

import programreader.expressions.main.functions.Function;
import programreader.expressions.special.Expression;
import programreader.expressions.special.MainExpression;
import programreader.expressions.special.Value;
import programreader.expressions.special.ValueHolder;
import programreader.program.ExpressionType;
import static helper.Output.*;

public class ReturnStatement extends MainExpression implements ValueHolder {

	private ValueHolder val = null;
	private Function myFunc = null;

	public ReturnStatement(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME);
	}

	/** The Returnvalue */
	@Override
	public Value getValue() {
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
