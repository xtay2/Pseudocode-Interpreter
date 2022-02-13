package expressions.normal.operators.comparative;

import datatypes.BoolValue;
import datatypes.Value;
import expressions.abstractions.ValueHolder;
import expressions.normal.operators.Operator;
import expressions.normal.operators.OperatorTypes.InfixOperator;

@FunctionalInterface
interface ComparativeOperation {
	BoolValue execute(Value a, Value b);
}

public final class ComparativeOperator extends Operator {

	private final ComparativeOperation operation;

	public ComparativeOperator(int line, InfixOperator operator) {
		super(line, operator);
		operation = switch (operator) {
		case EQUALS -> (a, b) -> Value.eq(a, b);
		case NOT_EQUALS -> (a, b) -> (Value.eq(a, b)).not();

		case LESS -> (a, b) -> new BoolValue(a.asNumber().isSmallerThan(b.asNumber()));
		case LESS_EQ -> (a, b) -> new BoolValue(a.asNumber().isSmallerEq(b.asNumber()));

		case GREATER -> (a, b) -> new BoolValue(a.asNumber().isGreaterThan(b.asNumber()));
		case GREATER_EQ -> (a, b) -> new BoolValue(a.asNumber().isGreaterEq(b.asNumber()));

		default -> throw new IllegalArgumentException("Unexpected value: " + operator);
		};
	}

	@Override
	public final Associativity getAssociativity() {
		return Associativity.NONE;
	}

	@Override
	public final Value perform(ValueHolder a, ValueHolder b) {
		return operation.execute(a.getValue(), b.getValue());
	}

}
