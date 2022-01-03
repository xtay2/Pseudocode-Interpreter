package expressions.normal.operators.logic;

import datatypes.BoolValue;
import datatypes.Value;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class NorOperator extends Operator {

	public NorOperator(int line, InfixOperator nor) {
		super(line, nor);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return BoolValue.nor(a.getValue().asBool(), b.getValue().asBool());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
