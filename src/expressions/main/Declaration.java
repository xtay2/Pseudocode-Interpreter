package expressions.main;

import static helper.Output.print;
import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;

import java.util.Arrays;

import datatypes.Value;
import exceptions.runtime.DeclarationException;
import exceptions.runtime.IllegalReturnException;
import expressions.normal.Name;
import expressions.normal.Semicolon;
import expressions.normal.Variable;
import expressions.normal.array.ArrayAccess;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.VarManager;

public class Declaration extends MainExpression {

	enum State {
		ARRAY_MODIFICATION, ASSIGNMENT, DECLARATION;
	}
	private ArrayAccess arr = null;
	private State state;
	private Name target = null;

	private ValueHolder val = null;

	private Variable var = null;

	public Declaration(int line) {
		super(line);
		setExpectedExpressions(LITERAL, NAME, ARRAY_START);
	}

	@Override
	public void build(Expression... args) {
		// Erstes argument ist var, bool, text oder nr.
		if (args[0] instanceof Variable && (args.length == 4 || (args.length == 5 && args[4] instanceof Semicolon))) {
			state = State.DECLARATION; // Initialisierung
			var = (Variable) args[0];
			target = (Name) args[1];
			VarManager.nameCheck(target.getName(), getOriginalLine());
			val = (ValueHolder) args[3];
			return;
		}
		if (args[0] instanceof Name && (args.length == 3 || (args.length == 4 && args[3] instanceof Semicolon))) {
			state = State.ASSIGNMENT; // Wertzuweisung
			target = (Name) args[0];
			val = (ValueHolder) args[2];
			return;
		}
		if (args[0] instanceof ArrayAccess a) {
			state = State.ARRAY_MODIFICATION;
			arr = a;
			target = a.name;
			val = (ValueHolder) args[2];
			return;
		}
		throw new DeclarationException(getOriginalLine(), "Illegal declaration. " //
				+ "Has to be something like: " //
				+ "\n\"name = value\", "//
				+ "\n\"var name = value\", "//
				+ "\n\"var[] name = value\", "//
				+ "\nor \"var[i] = value\""//
				+ "\nWas " + Arrays.toString(args));
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		try {
			Value value = val.getValue();
			if (state == State.DECLARATION) {
				print("Declaring " + target + " as " + value + " in scope: \"" + target.getScope().getScopeName() + "\"");
				var.initialise(target, value);
			} else if (state == State.ASSIGNMENT) {
				print("Changing the value of " + target + " to " + value);
				VarManager.get(target.getName(), getOriginalLine()).setValue(value);
			} else if (state == State.ARRAY_MODIFICATION) {
				arr.setValue(value);
			}
		} catch (NullPointerException e) {
			e.printStackTrace();
			throw new IllegalReturnException(getOriginalLine(), "Function has to return a value!");
		}
		return callNextLine(doExecuteNext);
	}

	public Name getName() {
		return target;
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "=";
	}
}
