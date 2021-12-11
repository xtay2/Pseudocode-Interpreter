package expressions.normal.operators.logic;

import datatypes.BoolValue;
import datatypes.Castable;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class NorOperator extends Operator {

	public NorOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Castable perform(ValueHolder a, ValueHolder b) {
		return BoolValue.nor(a.getValue().asBool(), b.getValue().asBool());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
