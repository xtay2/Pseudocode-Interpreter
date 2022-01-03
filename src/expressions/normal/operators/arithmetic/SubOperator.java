package expressions.normal.operators.arithmetic;

import datatypes.Value;
import datatypes.NumberValue;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class SubOperator extends Operator {

	public SubOperator(int line, InfixOperator sub) {
		super(line, sub);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value fst = a.getValue();
		Value sec = b.getValue();
		return NumberValue.sub(fst.asNumber(), sec.asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.LEFT;
	}

}
