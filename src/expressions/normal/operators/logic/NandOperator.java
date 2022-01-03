package expressions.normal.operators.logic;

import datatypes.BoolValue;
import datatypes.Value;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

public class NandOperator extends Operator {

	public NandOperator(int line, InfixOperator nand) {
		super(line, nand);
	}

	@Override
	public Value perform(ValueHolder a, ValueHolder b) {
		return BoolValue.nand(a.getValue().asBool(), b.getValue().asBool());
	}

	@Override
	public Associativity getAssociativity() {
		return Associativity.NONE;
	}

}
