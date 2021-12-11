package expressions.main;

import static helper.Output.print;

import java.util.Arrays;

import datatypes.ArrayValue;
import datatypes.Castable;
import exceptions.DeclarationException;
import expressions.normal.Literal;
import expressions.normal.Name;
import expressions.normal.Variable;
import expressions.normal.array.ArrayEnd;
import expressions.normal.array.ArrayStart;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.ValueHolder;
import interpreter.VarManager;
import parser.program.ExpressionType;

public class Declaration extends MainExpression {

	private Variable var = null;
	private ValueHolder val = null;
	private Name name = null;

	private State state;

	enum State {
		DECLARATION, ASSIGNMENT
	}

	public Declaration(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME, ExpressionType.ARRAY_START);
	}

	@Override
	public void build(Expression... args) {
		// Erstes argument ist var, bool, text oder nr.
		if ((args[0] instanceof Variable && args.length == 4)) {
			state = State.DECLARATION; // Initialisierung
			var = (Variable) args[0];
			name = (Name) args[1];
			VarManager.nameCheck(name.getName());
			val = (ValueHolder) args[3];
			return;
		}
		// Array Declaration
		if (args[0] instanceof Variable && args[1] instanceof ArrayStart && args[2] instanceof ArrayEnd && args.length == 6) {
			state = State.DECLARATION;
			var = (Variable) args[0];
			name = (Name) args[3];
			VarManager.nameCheck(name.getName());
			val = (((Literal) args[5]).getValue().as(var.getType()));
			((ArrayValue) val).init();
			return;
		}
		if (args[0] instanceof Name && args.length == 3) {
			state = State.ASSIGNMENT; // Wertzuweisung
			name = (Name) args[0];
			val = (ValueHolder) args[2];
			// declarationTarget ist hier unbekannt. Muss zur Laufzeit erfragt werden.
			return;
		}
		throw new DeclarationException(
				"Illegal declaration. \nHas to be something like: \"name = value\", \"var name = value\" or \"var[] name = value\"" + "\nWas " + Arrays.toString(args));
	}

	public Name getName() {
		return name;
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		try {
			Castable value = val.getValue();
			if (state == State.DECLARATION) {
				print("Declaring " + name + " as " + value + " in scope: \"" + name.getScope().getScopeName() + "\"");
				var.initialise(name, value);
			} else if (state == State.ASSIGNMENT) {
				print("Changing the value of " + name + " to " + value);
				VarManager.get(name.getName()).setValue(value);
			}
		} catch (NullPointerException e) {
			e.printStackTrace();
			throw new DeclarationException("Function has to return a value!");
		}
		return callNextLine(doExecuteNext);
	}
}
