package expressions.normal.operators.arithmetic;

import datatypes.Value;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.operators.Operator;
import expressions.normal.operators.OperatorTypes.InfixOperator;

public class DivOperator extends Operator {

	public DivOperator(int line, InfixOperator div) {
		super(line, div);
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.LEFT;
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return a.getValue().asNumber().div(b.getValue().asNumber());
	}

}
