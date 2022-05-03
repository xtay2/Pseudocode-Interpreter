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
		Value bVal = b.getValue();
		return switch (op) {
			case EQUALS -> Value.eq(aVal, bVal);
			case NOT_EQUALS -> Value.eq(aVal, bVal).not();

			case LESS -> valueOf(aVal.asNr().isSmallerThan(bVal.asNr()));
			case LESS_EQ -> valueOf(aVal.asNr().isSmallerEq(bVal.asNr()));

			case GREATER -> valueOf(aVal.asNr().isGreaterThan(bVal.asNr()));
			case GREATER_EQ -> valueOf(aVal.asNr().isGreaterEq(bVal.asNr()));

			default -> throw new IllegalArgumentException("Unexpected value: " + op);
		};
	}
}
