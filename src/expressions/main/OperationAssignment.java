package expressions.main;

import static helper.Output.print;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;
import static parsing.program.ExpressionType.OPEN_BRACKET;

import java.util.ArrayList;
import java.util.List;

import datatypes.Value;
import exceptions.runtime.DeclarationException;
import exceptions.runtime.IllegalReturnException;
import expressions.normal.Name;
import expressions.normal.Semicolon;
import expressions.normal.operators.Operation;
import expressions.normal.operators.Operator;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.VarManager;

/**
 * Similar to the {@link Declaration}, but this one modifies the value before it
 * gets declared.
 */
public class OperationAssignment extends MainExpression {

	public static enum Type {
		ADDI("+="), DIVI("/="), MODI("%="), MULTI("*="), POWI("^="), SUBI("-=");

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
	private final Type type;
	private ValueHolder val;

	public OperationAssignment(int line, Type type) {
		super(line);
		setExpectedExpressions(LITERAL, NAME, OPEN_BRACKET);
		this.type = type;
		op = Operator.operatorExpression(type.label.substring(0, 1), line);
	}

	@Override
	public void build(Expression... args) {
		if (args.length == 3 || (args.length == 4 && args[3] instanceof Semicolon)) {
			if (args[0] instanceof Name n && args[2] instanceof ValueHolder v) {
				target = n;
				val = v;
			} else
				throw new DeclarationException(lineIdentifier,
						"Operation-declaration has to follow the structure: [Name] [Declarative Operator] [Value]");
		}
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		try {
			List<Expression> operation = new ArrayList<>(3);
			operation.add(VarManager.get(target.getName(), lineIdentifier));
			operation.add(op);
			operation.add(val.getValue());
			Value value = new Operation(lineIdentifier, operation).getValue();
			print("Changing the value of " + target + " to " + value);
			VarManager.get(target.getName(), getOriginalLine()).setValue(value);
		} catch (NullPointerException e) {
			e.printStackTrace();
			throw new IllegalReturnException(getOriginalLine(), "Function has to return a value!");
		}
		return callNextLine(doExecuteNext);
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : type.toString();
	}

}
