package expressions.normal.operators.arithmetic;

import datatypes.Castable;
import datatypes.NumberValue;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class PowOperator extends Operator {

	public PowOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Castable perform(ValueHolder a, ValueHolder b) {
		return NumberValue.pow(a.getValue().asNumber(), b.getValue().asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}
}
