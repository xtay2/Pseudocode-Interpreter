package expressions.normal.operators.arithmetic;

import datatypes.NumberValue;
import datatypes.Value;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

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
		return NumberValue.root(a.getValue().asNumber(), b.getValue().asNumber());
	}
}
