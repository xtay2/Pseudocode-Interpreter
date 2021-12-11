package expressions.main.statements;

import static helper.Output.print;

import expressions.special.ValueHolder;
import interpreter.Interpreter;
import interpreter.VarManager;

public class ElifStatement extends IfStatement implements ElifConstruct {

	public ElifStatement(int line) {
		super(line);
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Executing Elif-Statement.");
		if (!doExecuteNext)
			throw new IllegalStateException("An elif-statement has to be able to call the next line.");
		if (booleanExp.getValue().asBool().rawBoolean()) {
			VarManager.registerScope(this);
			if (!Interpreter.execute(line + 1, !isOneLineStatement())) {
				VarManager.deleteScope(this);
				return false; // Wenn durch return abgebrochen wurde, rufe nichts hinter dem Block auf.
			}
			VarManager.deleteScope(this);
		} else if (nextElse != null && !Interpreter.execute(nextElse.getStart(), true))
			return false;
		return (nextElse == null ? true : Interpreter.execute(endOfConstruct(), true));
	}

	@Override
	public String getScopeName() {
		return "elif" + getStart() + "-" + getEnd();
	}

	/** Initialises the following elif / else statement. */
	public void setNextElse(ElifConstruct nextElse) {
		if (this.nextElse != null)
			throw new IllegalArgumentException("Trying to connect more than one else to this elif.");
		this.nextElse = nextElse;
	}
}
