package expressions.normal.operators.logic;

import datatypes.BoolValue;
import datatypes.Value;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class XorOperator extends Operator {

	public XorOperator(int line, InfixOperator xor) {
		super(line, xor);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return BoolValue.xor(a.getValue().asBool(), b.getValue().asBool());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
