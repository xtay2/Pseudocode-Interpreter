package expressions.main;

import static helper.Output.print;
import static types.specific.BuilderType.ARRAY_START;
import static types.specific.BuilderType.OPEN_BRACKET;
import static types.specific.ExpressionType.LITERAL;
import static types.specific.ExpressionType.NAME;
import static types.specific.ExpressionType.OPERATION_ASSIGNMENT;

import java.util.ArrayList;
import java.util.List;

import datatypes.Value;
import exceptions.runtime.IllegalReturnException;
import expressions.abstractions.Expression;
import expressions.abstractions.MainExpression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.operators.Operation;
import expressions.normal.operators.infix.InfixOperator;
import modules.finder.ExpressionFinder;
import types.SuperType;
import types.specific.ExpressionType;

/**
 * Similar to the {@link Declaration}, but this one modifies the value before it gets declared.
 */
public class OperationAssignment extends MainExpression implements MergedExpression {

	public static enum Type {
		ADDI("+="), SUBI("-="), MULTI("*="), DIVI("/="), POWI("^="), MODI("%=");

		public final String label;

		private Type(String label) {
			this.label = label;
		}
	}

	private final InfixOperator op;

	private ValueChanger target;
	private ValueHolder val;

	private OperationAssignment(int line, Type type) {
		super(line, OPERATION_ASSIGNMENT, LITERAL, NAME, OPEN_BRACKET, ARRAY_START);
		op = (InfixOperator) ExpressionFinder.find(type.label.replace("=", ""), line, SuperType.INFIX_OPERATOR);
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
	public boolean execute(ValueHolder... params) {
		try {
			List<Expression> operation = new ArrayList<>(3);
			operation.add(target.getValue());
			operation.add(op);
			operation.add(val.getValue());
			Operation op = new Operation(lineIdentifier);
			op.merge(operation);
			Value value = op.getValue();
			print("Changing the value of " + target + " to " + value);
			target.setValue(value);
		} catch (NullPointerException e) {
			e.printStackTrace();
			throw new IllegalReturnException(getOriginalLine(), "Function has to return a value!");
		}
		return callNextLine();
	}

	// Static Methods

	/**
	 * Builds a {@link OperationAssignment} from a {@link String}.
	 * 
	 * Called in {@link ExpressionType#create(String, int)}
	 * 
	 * Returns null if the {@link String} doesn't match any {@link Type}.
	 */
	public static OperationAssignment stringToOpAssign(String arg, int lineID) {
		for (Type t : Type.values()) {
			if (t.label.equals(arg))
				return new OperationAssignment(lineID, t);
		}
		return null;
	}
}
