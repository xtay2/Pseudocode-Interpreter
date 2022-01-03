package expressions.normal.operators.logic;

import datatypes.BoolValue;
import datatypes.Value;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class OrOperator extends Operator{

	public OrOperator(int line, InfixOperator or) {
		super(line, or);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return BoolValue.or(a.getValue().asBool(), b.getValue().asBool());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}
}
