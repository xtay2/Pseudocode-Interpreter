package expressions.normal.operators.comparative;

import datatypes.Value;
import datatypes.NumberValue;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class GreaterOperator extends Operator implements ComparativeOperator{

	public GreaterOperator(int line, InfixOperator greater) {
		super(line, greater);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return NumberValue.isSmallerThan(b.getValue().asNumber(), a.getValue().asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
