package expressions.normal.operators.comparative;

import datatypes.BoolValue;
import datatypes.NumberValue;
import datatypes.Value;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

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

		case LESS -> (a, b) -> NumberValue.isSmallerThan(a.asNumber(), b.asNumber());
		case LESS_EQ -> (a, b) -> NumberValue.isSmallerEq(a.asNumber(), b.asNumber());

		case GREATER -> (a, b) -> NumberValue.isSmallerEq(a.asNumber(), b.asNumber()).not();
		case GREATER_EQ -> (a, b) -> NumberValue.isSmallerThan(a.asNumber(), b.asNumber()).not();

		default -> throw new IllegalArgumentException("Unexpected value: " + operator);
		};
	}

	@Override
	public final Associativity getAssociativity() {
		return Associativity.NONE;
	}

	@Override
	public final Value perform(ValueHolder a, ValueHolder b) {
		return operation.execute(a.getValue().asBool(), b.getValue().asBool());
	}

}
