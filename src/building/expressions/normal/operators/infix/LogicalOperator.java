package building.expressions.normal.operators.infix;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.operators.InfixOpType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;

@FunctionalInterface
interface LogicalOperation {
	BoolValue execute(BoolValue a, BoolValue b);
}

public final class LogicalOperator extends InfixOperator {

	private final LogicalOperation operation;

	public LogicalOperator(int lineID, InfixOpType operator) {
		super(lineID, operator);
		operation = switch (operator) {
			case AND -> (b1, b2) -> BoolValue.valueOf(b1.value && b2.value);
			case NAND -> (b1, b2) -> BoolValue.valueOf(!(b1.value && b2.value));

			case OR -> (b1, b2) -> BoolValue.valueOf(b1.value || b2.value);
			case NOR -> (b1, b2) -> BoolValue.valueOf(!(b1.value || b2.value));

			case XOR -> (b1, b2) -> BoolValue.valueOf(b1.value ^ b2.value);
			case XNOR -> (b1, b2) -> BoolValue.valueOf(b1.value == b2.value);

			default -> throw new IllegalArgumentException("Unexpected value: " + operator);
		};
	}

	@Override
	public final Value perform(ValueHolder a, ValueHolder b) {
		return operation.execute(a.getValue().asBool(), b.getValue().asBool());
	}

}