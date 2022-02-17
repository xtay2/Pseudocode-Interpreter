package expressions.normal.operators.arithmetic;

import datatypes.Value;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.operators.Operator;
import expressions.normal.operators.OperatorTypes.InfixOperator;

public class ModOperator extends Operator {

	public ModOperator(int line, InfixOperator mod) {
		super(line, mod);
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.LEFT;
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return a.getValue().asNumber().mod(b.getValue().asNumber());
	}
}
