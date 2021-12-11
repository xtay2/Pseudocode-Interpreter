package expressions.normal.operators.logic;

import datatypes.BoolValue;
import datatypes.Castable;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class AndOperator extends Operator {

	public AndOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Castable perform(ValueHolder a, ValueHolder b) {
		return BoolValue.and(a.getValue().asBool(), b.getValue().asBool());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
