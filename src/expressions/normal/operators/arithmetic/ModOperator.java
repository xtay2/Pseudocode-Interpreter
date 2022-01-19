package expressions.normal.operators.arithmetic;

import datatypes.NumberValue;
import datatypes.Value;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

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
		Value fst = a.getValue();
		Value sec = b.getValue();
		return NumberValue.mod(fst.asNumber(), sec.asNumber());
	}
}
