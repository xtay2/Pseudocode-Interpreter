package expressions.main;

import static helper.Output.print;
import static types.ExpressionType.LITERAL;
import static types.ExpressionType.NAME;
import static types.specific.BuilderType.OPEN_BRACKET;
import java.util.ArrayList;
import java.util.List;

import datatypes.Value;
import exceptions.runtime.IllegalReturnException;
import expressions.abstractions.Expression;
import expressions.abstractions.MainExpression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.containers.Name;
import expressions.normal.operators.Operation;
import expressions.normal.operators.Operator;
import interpreter.VarManager;
import types.ExpressionType;

/**
 * Similar to the {@link Declaration}, but this one modifies the value before it
 * gets declared.
 */
public class OperationAssignment extends MainExpression implements MergedExpression {

	public static enum Type {
		ADDI("+="), SUBI("-="), MULTI("*="), DIVI("/="), POWI("^="), MODI("%=");

		public static Type getType(String arg) {
			for (Type t : values()) {
				if (t.label.equals(arg))
					return t;
			}
			return null;
		}

		public final String label;

		Type(String label) {
			this.label = label;
		}
	}

	private Operator op;

	private Name target;
	private ValueHolder val;

	public OperationAssignment(int line, Type type) {
		super(line, ExpressionType.OPERATION_ASSIGNMENT, LITERAL, NAME, OPEN_BRACKET);
		op = Operator.operatorExpression(type.label.substring(0, 1), line);
	}

	/** [NAME] [VALUE_HOLDER] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 2)
			throw new AssertionError("Merge on a OperationAssignment has to contain a Name and a ValueHolder.");
		target = (Name) e[0];
		val = (ValueHolder) e[1];
	}

	@Override
	public boolean execute(ValueHolder... params) {
		try {
			List<Expression> operation = new ArrayList<>(3);
			operation.add(VarManager.get(target.getName(), lineIdentifier));
			operation.add(op);
			operation.add(val.getValue());
			Operation op = new Operation(lineIdentifier);
			op.merge(operation);
			Value value = op.getValue();
			print("Changing the value of " + target + " to " + value);
			VarManager.get(target.getName(), getOriginalLine()).setValue(value);
		} catch (NullPointerException e) {
			e.printStackTrace();
			throw new IllegalReturnException(getOriginalLine(), "Function has to return a value!");
		}
		return callNextLine();
	}
}
