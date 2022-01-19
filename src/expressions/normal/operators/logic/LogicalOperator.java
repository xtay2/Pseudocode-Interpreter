package expressions.normal.operators.logic;

import datatypes.BoolValue;
import datatypes.Value;
import expressions.normal.operators.InfixOperator;
import expressions.normal.operators.Operator;
import expressions.special.ValueHolder;

@FunctionalInterface
interface LogicalOperation {
	BoolValue execute(BoolValue a, BoolValue b);
}

public final class LogicalOperator extends Operator {

	private final LogicalOperation operation;

	public LogicalOperator(int line, InfixOperator operator) {
		super(line, operator);
		operation = switch (operator) {
		case AND -> (b1, b2) -> new BoolValue(b1.raw() && b2.raw());
		case NAND -> (b1, b2) -> new BoolValue(!(b1.raw() && b2.raw()));

		case OR -> (b1, b2) -> new BoolValue(b1.raw() || b2.raw());
		case NOR -> (b1, b2) -> new BoolValue(!(b1.raw() || b2.raw()));

		case XOR -> (b1, b2) -> new BoolValue(b1.raw() ^ b2.raw());
		case XNOR -> (b1, b2) -> new BoolValue(b1.raw() == b2.raw());

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