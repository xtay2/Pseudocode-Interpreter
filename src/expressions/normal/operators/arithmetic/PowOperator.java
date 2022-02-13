package expressions.normal.operators.arithmetic;

import datatypes.NumberValue;
import datatypes.Value;
import expressions.abstractions.ValueHolder;
import expressions.normal.operators.Operator;
import expressions.normal.operators.OperatorTypes.InfixOperator;

public class PowOperator extends Operator {

	public PowOperator(int line, InfixOperator pow) {
		super(line, pow);
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return NumberValue.pow(a.getValue().asNumber(), b.getValue().asNumber());
	}
}
