package building.expressions.normal.operators.infix;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.operators.InfixOpType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;

@FunctionalInterface
interface ComparativeOperation {
	BoolValue execute(Value a, Value b);
}

public final class ComparativeOperator extends InfixOperator {

	private final ComparativeOperation operation;

	public ComparativeOperator(int lineID, InfixOpType operator) {
		super(lineID, operator);
		operation = switch (operator) {
			case EQUALS -> (a, b) -> Value.eq(a, b);
			case NOT_EQUALS -> (a, b) -> Value.eq(a, b).not();

			case LESS -> (a, b) -> BoolValue.valueOf(a.asNumber().isSmallerThan(b.asNumber()));
			case LESS_EQ -> (a, b) -> BoolValue.valueOf(a.asNumber().isSmallerEq(b.asNumber()));

			case GREATER -> (a, b) -> BoolValue.valueOf(a.asNumber().isGreaterThan(b.asNumber()));
			case GREATER_EQ -> (a, b) -> BoolValue.valueOf(a.asNumber().isGreaterEq(b.asNumber()));

			default -> throw new IllegalArgumentException("Unexpected value: " + operator);
		};
	}

	@Override
	public final Value perform(ValueHolder a, ValueHolder b) {
		return operation.execute(a.getValue(), b.getValue());
	}

}
