package expressions.normal.operators.arithmetic;

import datatypes.Value;
import datatypes.NumberValue;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class PowOperator extends Operator {

	public PowOperator(int line, InfixOperator pow) {
		super(line, pow);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return NumberValue.pow(a.getValue().asNumber(), b.getValue().asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}
}
