package expressions.main.statements;

import static parsing.program.ExpressionType.EXPECTED_TYPE;

import datatypes.BoolValue;
import datatypes.Value;
import expressions.normal.ExpectedType;
import expressions.normal.Expression;
import expressions.special.DataType;
import expressions.special.MergedExpression;
import expressions.special.ValueHolder;

/**
 * Nearly identical to instanceof in Java. Checks if a value is an instance of a
 * given type.
 */
public class IsStatement extends Expression implements ValueHolder, MergedExpression, Statement {

	DataType type;
	ValueHolder val;

	public IsStatement(int line) {
		super(line);
		setExpectedExpressions(EXPECTED_TYPE);
	}

	@Override
	public void merge(Expression... e) {
		val = (ValueHolder) e[0];
		type = ((ExpectedType) e[1]).type;
	}

	@Override
	public Value getValue() {
		return new BoolValue(val.getValue().getType() == type);
	}

}
