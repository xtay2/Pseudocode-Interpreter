package expressions.normal.operators.arithmetic;

import datatypes.Value;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.operators.Operator;
import expressions.normal.operators.OperatorTypes.InfixOperator;

public class SubOperator extends Operator {

	public SubOperator(int line, InfixOperator sub) {
		super(line, sub);
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.LEFT;
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return a.getValue().asNumber().sub(b.getValue().asNumber());
	}

}
