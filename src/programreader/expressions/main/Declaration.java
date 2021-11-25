package programreader.expressions.main;

import exceptions.DeclarationException;
import programreader.expressions.normal.Name;
import programreader.expressions.normal.Variable;
import programreader.expressions.special.Expression;
import programreader.expressions.special.MainExpression;
import programreader.expressions.special.Value;
import programreader.expressions.special.ValueHolder;
import programreader.interpreter.VarManager;
import programreader.program.ExpressionType;
import static helper.Output.*;

import java.util.Arrays;

public class Declaration extends MainExpression {

	private Variable declarationTarget = null;
	private ValueHolder val = null;
	private Name name = null;

	private State state;

	enum State {
		DECLARATION, ASSIGNMENT
	}

	public Declaration(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME);
	}

	@Override
	public void build(Expression... args) {
		// Erstes argument ist var, bool, text oder nr.
		if ((args[0] instanceof Variable) && args.length == 4) {
			state = State.DECLARATION; // Initialisierung
			declarationTarget = (Variable) args[0];
			name = (Name) args[1];
			val = (ValueHolder) args[3];
			return;
		} else if (args[0] instanceof Name && args.length == 3) {
			state = State.ASSIGNMENT; // Wertzuweisung
			name = (Name) args[0];
			val = (ValueHolder) args[2];
			// declarationTarget ist hier unbekannt. Muss zur Laufzeit erfragt werden.
			return;
		}
		throw new DeclarationException(
				"Illegal declaration. Has to be something like: \"name = value\" or \"var name value\"" + "\nWas "
						+ Arrays.toString(args));
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
				declarationTarget.initialise(name, value);
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
