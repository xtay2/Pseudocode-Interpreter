package expressions.normal.operators.arithmetic;

import datatypes.Value;
import expressions.abstractions.interfaces.ValueHolder;
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
		return a.getValue().asNumber().pow(b.getValue().asNumber());
	}
}
