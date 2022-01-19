package expressions.main.statements;

import static helper.Output.print;
import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;

import expressions.normal.brackets.OpenBlock;
import expressions.special.Expression;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.Interpreter;
import interpreter.VarManager;

public class IfStatement extends Scope implements ElifConstruct {

	protected ValueHolder booleanExp = null;
	protected ElifConstruct nextElse = null;

	public IfStatement(int line) {
		super(line);
		setExpectedExpressions(LITERAL, NAME, ARRAY_START);
	}

	@Override
	public void build(Expression... args) {
		booleanExp = (ValueHolder) args[1];
		if (args[args.length - 1] instanceof OpenBlock)
			block = (OpenBlock) args[args.length - 1];
	}

	@Override
	public int endOfConstruct() {
		if (nextElse != null)
			return nextElse.endOfConstruct();
		return getEnd();
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Executing If-Statement.");
		if (!doExecuteNext)
			throw new AssertionError("An if-statement has to be able to call the next line.");
		if (booleanExp.getValue().asBool().raw()) {
			VarManager.registerScope(this);
			if (!Interpreter.execute(lineIdentifier + 1, true)) {
				VarManager.deleteScope(this);
				return false; // Wenn durch return abgebrochen wurde, rufe nichts hinter dem Block auf.
			}
			VarManager.deleteScope(this);
		} else if (nextElse != null && !Interpreter.execute(((Scope) nextElse).getStart(), true))
			return false;
		return Interpreter.execute(nextElse == null ? getEnd() : endOfConstruct(), true);
	}

	@Override
	public String getScopeName() {
		return "if" + getStart() + "-" + getEnd();
	}

	/** Initialises the following elif / else statement. */
	@Override
	public void setNextElse(ElifConstruct nextElse) {
		if (this.nextElse != null)
			throw new AssertionError("Trying to connect more than one else to this statement.");
		this.nextElse = nextElse;
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "if";
	}
}