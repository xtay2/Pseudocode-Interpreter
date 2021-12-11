package expressions.normal.operators.comparative;

import datatypes.Castable;
import datatypes.NumberValue;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class LessEqOperator extends Operator implements ComparativeOperator {

	public LessEqOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Castable perform(ValueHolder a, ValueHolder b) {
		return NumberValue.isSmallerEq(a.getValue().asNumber(), b.getValue().asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
