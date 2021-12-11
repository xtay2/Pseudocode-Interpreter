package expressions.normal.operators.comparative;

import datatypes.Castable;
import datatypes.NumberValue;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class GreaterEqOperator extends Operator implements ComparativeOperator {

	public GreaterEqOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Castable perform(ValueHolder a, ValueHolder b) {
		return NumberValue.isSmallerEq(b.getValue().asNumber(), a.getValue().asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
