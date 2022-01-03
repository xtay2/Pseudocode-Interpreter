package expressions.normal.operators.comparative;

import datatypes.Value;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class NotEqualsOperator extends Operator implements ComparativeOperator {

	public NotEqualsOperator(int line, InfixOperator notEquals) {
		super(line, notEquals);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return a.getValue().neq(b.getValue());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
