package expressions.possible.assigning;

import static helper.Output.print;

import java.util.List;

import datatypes.Value;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.operators.Operation;
import expressions.normal.operators.infix.ArithmeticOperator;
import expressions.normal.operators.infix.ComparativeOperator;
import expressions.normal.operators.infix.InOperator;
import expressions.normal.operators.infix.InfixOperator;
import expressions.normal.operators.infix.LogicalOperator;
import types.specific.AssignmentType;

/**
 * Similar to the {@link Declaration}, but this one modifies the value before it gets declared.
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
				case EQUALS, NOT_EQUALS, GREATER, GREATER_EQ, LESS, LESS_EQ -> new ComparativeOperator(lineIdentifier, type.op);
				// Logical
				case AND, NAND, OR, NOR, XOR, XNOR -> new LogicalOperator(lineIdentifier, type.op);
				// Misc
				case IN -> new InOperator(lineIdentifier, type.op);
			};
		}
	}

	@Override
	public Value getValue() {
		Value value;
		if (op != null)
			value = new Operation(lineIdentifier, List.of(target.getValue(), op, val.getValue())).getValue();
		else
			value = val.getValue();
		print("Changing the value of " + target + " to " + value);
		target.setValue(value);
		return value;
	}
}
