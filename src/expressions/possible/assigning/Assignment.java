package expressions.possible.assigning;

import static helper.Output.print;
import static types.SuperType.ASSIGNMENT_TYPE;
import static types.specific.BuilderType.ARRAY_START;
import static types.specific.BuilderType.OPEN_BRACKET;
import static types.specific.ExpressionType.LITERAL;
import static types.specific.ExpressionType.NAME;

import java.util.ArrayList;
import java.util.List;

import datatypes.Value;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.operators.Operation;
import expressions.normal.operators.infix.InfixOperator;
import types.specific.AssingmentType;

/**
 * Similar to the {@link Declaration}, but this one modifies the value before it gets declared.
 */
public class Assignment extends Allocating {

	private final InfixOperator op;

	public Assignment(int lineID, AssingmentType type) {
		super(lineID, ASSIGNMENT_TYPE, LITERAL, NAME, OPEN_BRACKET, ARRAY_START);
		op = type.op == null ? null : type.op.create(type.label.replace("=", ""), lineID);
	}

	/** [NAME] [VALUE_HOLDER] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 2)
			throw new AssertionError("Merge on a OperationAssignment has to contain a Name and a ValueHolder.");
		target = (ValueChanger) e[0];
		val = (ValueHolder) e[1];
	}

	@Override
	public Value getValue() {
		Value value;
		if (op != null) {
			List<Expression> operation = new ArrayList<>(3);
			operation.add(target.getValue());
			operation.add(op);
			operation.add(val.getValue());
			Operation op = new Operation(lineIdentifier);
			op.merge(operation);
			value = op.getValue();
		} else
			value = val.getValue();
		print("Changing the value of " + target + " to " + value);
		target.setValue(value);
		return value;
	}
}
