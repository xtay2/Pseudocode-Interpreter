package expressions.normal.operators.comparative;

import datatypes.Value;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class EqualsOperator extends Operator implements ComparativeOperator {

	public EqualsOperator(int line, InfixOperator equals) {
		super(line, equals);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return a.getValue().eq(b.getValue());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
