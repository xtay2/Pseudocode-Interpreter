package expressions.normal.operators.logic;

import datatypes.BoolValue;
import datatypes.Value;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class AndOperator extends Operator {

	public AndOperator(int line, InfixOperator and) {
		super(line, and);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return BoolValue.and(a.getValue().asBool(), b.getValue().asBool());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
