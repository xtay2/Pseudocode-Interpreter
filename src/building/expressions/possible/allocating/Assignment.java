package building.expressions.possible.allocating;

import static misc.helper.Output.print;

import java.util.List;

import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.containers.Variable;
import building.expressions.normal.operators.Operation;
import building.expressions.normal.operators.infix.*;
import building.types.specific.AssignmentType;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;

/**
 * Assigns a value to a {@link Variable} thats already initialised, and returns
 * the {@link Value} afterwards.
 */
public class Assignment extends Allocating {

	private final InfixOperator op;

	/**
	 * Creates an {@link Assignment}.
	 * 
	 * @param type   shouldn't be null.
	 * @param target shouldn't be null.
	 * @param val    shouldn't be null.
	 */
	public Assignment(int lineID, AssignmentType type, ValueChanger target, ValueHolder val) {
		super(lineID, type, target, val);
		if (type.op == null)
			op = null;
		else {
			op = switch (type.op) {
			// Arithmetic
			case ADD, SUB, MULT, DIV, MOD, POW, ROOT -> new ArithmeticOperator(lineIdentifier, type.op);
			// Comparison
			case EQUALS, NOT_EQUALS, GREATER, GREATER_EQ, LESS, LESS_EQ -> new ComparativeOperator(lineIdentifier,
					type.op);
			// Logical
			case AND, NAND, OR, NOR, XOR, XNOR -> new LogicalOperator(lineIdentifier, type.op);
			// Misc
			case IN -> new InOperator(lineIdentifier, type.op);
			};
		}
	}

	@Override
	public Value getValue() {
		Value value = val.getValue();
		if (value instanceof ArrayValue av)
			av.init();
		if (op != null)
			value = new Operation(lineIdentifier, List.of(target.getValue(), op, val.getValue())).getValue();
		print("Changing the value of " + target + " to " + value);
		target.setValue(value);
		return value;
	}
}
