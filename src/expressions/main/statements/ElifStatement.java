package expressions.main.statements;

import static helper.Output.print;

import expressions.abstractions.Scope;
import expressions.abstractions.ValueHolder;
import interpreter.Interpreter;
import interpreter.VarManager;
public class ElifStatement extends IfStatement implements ElifConstruct {

	public ElifStatement(int line) {
		super(line);
	}

	@Override
	public boolean execute(ValueHolder... params) {
		print("Executing Elif-Statement.");
		if (booleanExp.getValue().asBool().raw()) {
			VarManager.registerScope(this);
			if (!Interpreter.execute(lineIdentifier + 1)) {
				VarManager.deleteScope(this);
				return false; // Wenn durch return abgebrochen wurde, rufe nichts hinter dem Block auf.
			}
			VarManager.deleteScope(this);
		} else if (nextElse != null && !Interpreter.execute(((Scope) nextElse).getStart()))
			return false;
		return (nextElse == null ? true : Interpreter.execute(endOfConstruct()));
	}

	@Override
	public String getScopeName() {
		return "elif" + getStart() + "-" + getEnd();
	}
}
