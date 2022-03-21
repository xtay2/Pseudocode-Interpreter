package building.expressions.normal.operators.infix;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.operators.InfixOpType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;

public final class LogicalOperator extends InfixOperator {

	public LogicalOperator(int lineID, InfixOpType operator) {
		super(lineID, operator);
	}

	@Override
	public final Value perform(ValueHolder a, ValueHolder b) {
		// Only a can get evaluated directly
		boolean aBool = a.getValue().asBool().raw();
		return switch (op) {
		case AND -> BoolValue.valueOf(aBool && b.getValue().asBool().raw());
		case NAND -> BoolValue.valueOf(!(aBool && b.getValue().asBool().raw()));

		case OR -> BoolValue.valueOf(aBool || b.getValue().asBool().raw());
		case NOR -> BoolValue.valueOf(!(aBool || b.getValue().asBool().raw()));

		case XOR -> BoolValue.valueOf(aBool ^ b.getValue().asBool().raw());
		case XNOR -> BoolValue.valueOf(aBool == b.getValue().asBool().raw());

		default -> throw new IllegalArgumentException("Unexpected value: " + op);
		};
	}

}