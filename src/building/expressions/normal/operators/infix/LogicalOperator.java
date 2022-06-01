package building.expressions.normal.operators.infix;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.operators.InfixOpType;
import errorhandeling.NonExpressionException;
import errorhandeling.PseudocodeException;
import runtime.datatypes.BoolValue;

public final class LogicalOperator extends InfixOperator {

	public LogicalOperator(int lineID, InfixOpType operator) {
		super(lineID, operator);
	}

	@Override
	public final BoolValue perform(ValueHolder a, ValueHolder b) {
		try {
			// Only a can get evaluated directly
			boolean aBool = a.asBool().raw();
			return switch (op) {
				case AND -> BoolValue.valueOf(aBool && b.asBool().raw());
				case NAND -> BoolValue.valueOf(!(aBool && b.asBool().raw()));

				case OR -> BoolValue.valueOf(aBool || b.asBool().raw());
				case NOR -> BoolValue.valueOf(!(aBool || b.asBool().raw()));

				case XOR -> BoolValue.valueOf(aBool ^ b.asBool().raw());
				case XNOR -> BoolValue.valueOf(aBool == b.asBool().raw());

				default -> throw new IllegalArgumentException("Unexpected value: " + op);
			};
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, getDataPath());
		}
	}
}