package expressions.normal.operators.arithmetic;

import datatypes.Value;
import datatypes.NumberValue;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class DivOperator extends Operator {

	public DivOperator(int line, InfixOperator div) {
		super(line, div);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value fst = a.getValue();
		Value sec = b.getValue();
		return NumberValue.div(fst.asNumber(), sec.asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.LEFT;
	}

}
