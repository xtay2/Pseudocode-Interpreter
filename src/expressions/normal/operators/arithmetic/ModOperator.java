package expressions.normal.operators.arithmetic;

import datatypes.Value;
import datatypes.NumberValue;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class ModOperator extends Operator {

	public ModOperator(int line, InfixOperator mod) {
		super(line, mod);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		Value fst = a.getValue();
		Value sec = b.getValue();
		return NumberValue.mod(fst.asNumber(), sec.asNumber());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.LEFT;
	}
}
