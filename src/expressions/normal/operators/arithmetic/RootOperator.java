package expressions.normal.operators.arithmetic;

import datatypes.Value;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.operators.Operator;
import expressions.normal.operators.OperatorTypes.InfixOperator;

public class RootOperator extends Operator {

	public RootOperator(int line, InfixOperator root) {
		super(line, root);
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return a.getValue().asNumber().root(b.getValue().asNumber());
	}
}
