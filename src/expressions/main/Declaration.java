package expressions.main;

import static helper.Output.print;

import java.util.Arrays;

import datatypes.Value;
import exceptions.runtime.DeclarationException;
import exceptions.runtime.IllegalReturnException;
import expressions.normal.Name;
import expressions.normal.Semikolon;
import expressions.normal.Variable;
import expressions.normal.array.ArrayAccess;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.VarManager;
import parsing.program.ExpressionType;

public class Declaration extends MainExpression {

	private Variable var = null;
	private ValueHolder val = null;
	private Name name = null;
	private ArrayAccess arr = null;

	private State state;

	enum State {
		DECLARATION, ASSIGNMENT, ARRAY_MODIFICATION;
	}

	public Declaration(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME, ExpressionType.ARRAY_START);
	}

	@Override
	public void build(Expression... args) {
		// Erstes argument ist var, bool, text oder nr.
		if ((args[0] instanceof Variable && args.length == 4 || (args.length == 5 && args[4] instanceof Semikolon))) {
			state = State.DECLARATION; // Initialisierung
			var = (Variable) args[0];
			name = (Name) args[1];
			VarManager.nameCheck(name.getName());
			val = (ValueHolder) args[3];
			return;
		}
		if (args[0] instanceof Name && args.length == 3 || (args.length == 4 && args[3] instanceof Semikolon)) {
			state = State.ASSIGNMENT; // Wertzuweisung
			name = (Name) args[0];
			val = (ValueHolder) args[2];
			return;
		}
		if (args[0]instanceof ArrayAccess a) {
			state = State.ARRAY_MODIFICATION;
			arr = a;
			name = a.name;
			val = (ValueHolder) args[2];
			return;
		}
		throw new DeclarationException("Illegal declaration. " //
				+ "Has to be something like: " //
				+ "\n\"name = value\", "//
				+ "\n\"var name = value\", "//
				+ "\n\"var[] name = value\", "//
				+ "\nor \"var[i] = value\""//
				+ "\nWas " + Arrays.toString(args));
	}

	public Name getName() {
		return name;
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		try {
			Value value = val.getValue();
			if (state == State.DECLARATION) {
				print("Declaring " + name + " as " + value + " in scope: \"" + name.getScope().getScopeName() + "\"");
				var.initialise(name, value);
			} else if (state == State.ASSIGNMENT) {
				print("Changing the value of " + name + " to " + value);
				VarManager.get(name.getName()).setValue(value);
			} else if (state == State.ARRAY_MODIFICATION) {
				arr.setValue(value);
			}
		} catch (NullPointerException e) {
			e.printStackTrace();
			throw new IllegalReturnException("Function has to return a value!");
		}
		return callNextLine(doExecuteNext);
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "=";
	}
}
