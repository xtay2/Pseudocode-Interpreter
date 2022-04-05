package building.expressions.normal.operators.infix;

import static runtime.datatypes.BoolValue.valueOf;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.operators.InfixOpType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;

public final class ComparativeOperator extends InfixOperator {

	public ComparativeOperator(int lineID, InfixOpType operator) {
		super(lineID, operator);
	}

	@Override
	public final BoolValue perform(ValueHolder a, ValueHolder b) {
		Value aVal = a.getValue();
		Value bVal = a.getValue();
		return switch (op) {
			case EQUALS -> Value.eq(aVal, bVal);
			case NOT_EQUALS -> Value.eq(aVal, bVal).not();

			case LESS -> valueOf(aVal.asNumber().isSmallerThan(bVal.asNumber()));
			case LESS_EQ -> valueOf(aVal.asNumber().isSmallerEq(bVal.asNumber()));

			case GREATER -> valueOf(aVal.asNumber().isGreaterThan(bVal.asNumber()));
			case GREATER_EQ -> valueOf(aVal.asNumber().isGreaterEq(bVal.asNumber()));

			default -> throw new IllegalArgumentException("Unexpected value: " + op);
		};
	}
}
