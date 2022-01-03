package expressions.normal.operators.comparative;

import datatypes.Value;
import datatypes.NumberValue;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class GreaterEqOperator extends Operator implements ComparativeOperator {

	public GreaterEqOperator(int line, InfixOperator greaterEq) {
		super(line, greaterEq);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return NumberValue.isSmallerEq(b.getValue().asNumber(), a.getValue().asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
