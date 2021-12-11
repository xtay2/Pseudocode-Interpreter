package expressions.normal.operators.arithmetic;

import datatypes.Castable;
import datatypes.NumberValue;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class DivOperator extends Operator {

	public DivOperator(int line, int rank) {
		super(line, rank);
	}

	@Override
	public Castable perform(ValueHolder a, ValueHolder b) {
		Castable fst = a.getValue();
		Castable sec = b.getValue();
		return NumberValue.div(fst.asNumber(), sec.asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.LEFT;
	}

}
