package expressions.normal.operators.comparative;

import datatypes.Value;
import datatypes.NumberValue;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class LessEqOperator extends Operator implements ComparativeOperator {

	public LessEqOperator(int line, InfixOperator lessEq) {
		super(line, lessEq);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return NumberValue.isSmallerEq(a.getValue().asNumber(), b.getValue().asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
