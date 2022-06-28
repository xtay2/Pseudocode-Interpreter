package building.expressions.possible.allocating;

import java.util.*;

import building.expressions.abstractions.interfaces.*;
import building.expressions.normal.containers.*;
import building.expressions.normal.operators.*;
import building.expressions.normal.operators.infix.*;
import building.types.specific.*;
import runtime.datatypes.*;
import runtime.datatypes.array.*;

/**
 * Assigns a value to a {@link Variable} thats already initialised, and returns the {@link Value}
 * afterwards.
 */
public class Assignment extends Allocating {
	
	private final InfixOperator op;
	
	/**
	 * Creates an {@link Assignment}.
	 *
	 * @param type shouldn't be null.
	 * @param target shouldn't be null.
	 * @param val shouldn't be null.
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
	
	/**
	 * Changes the value of {@link Allocating#target} to {@link Allocating#val}.
	 *
	 * If the target is an {@link ArrayAccess}, the previous {@link Value} in that {@link ArrayValue}
	 * gets returned.
	 *
	 * If the target is a {@link Variable}, the new {@link Value} gets returned.
	 */
	@Override
	public Value getValue() {
		Value value = val.getValue();
		if (target instanceof ArrayAccess aac)
			return aac.setValue(value);
		if (op != null)
			value = new Operation(lineIdentifier, List.of(target.getValue(), op, value)).getValue();
		target.setValue(value);
		return value;
	}
}
