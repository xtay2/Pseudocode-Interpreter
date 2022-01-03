package expressions.normal.operators.comparative;

import datatypes.Value;
import datatypes.NumberValue;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class LessOperator extends Operator implements ComparativeOperator {

	public LessOperator(int line, InfixOperator less) {
		super(line, less);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return NumberValue.isSmallerThan(a.getValue().asNumber(), b.getValue().asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
