package expressions.main.statements;

import static helper.Output.print;
import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;

import expressions.normal.Expression;
import expressions.normal.brackets.OpenScope;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import interpreter.Interpreter;
import interpreter.VarManager;

public class IfStatement extends Scope implements ElifConstruct {

	protected ValueHolder booleanExp;
	protected ElifConstruct nextElse;

	public IfStatement(int line) {
		super(line);
		setExpectedExpressions(LITERAL, NAME, ARRAY_START);
	}

	/** [IF] [VALUEHOLDER] [OPEN_SCOPE] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 2)
			throw new AssertionError("If-Statement needs a boolean-expression and an opened scope.");
		booleanExp = (ValueHolder) e[0];
		block = (OpenScope) e[1];
	}

	@Override
	public int endOfConstruct() {
		if (nextElse != null)
			return nextElse.endOfConstruct();
		return getEnd();
	}

	@Override
	public boolean execute(ValueHolder... params) {
		print("Executing If-Statement.");
		if (booleanExp.getValue().asBool().raw()) {
			VarManager.registerScope(this);
			if (!Interpreter.execute(lineIdentifier + 1)) {
				VarManager.deleteScope(this);
				return false; // Wenn durch return abgebrochen wurde, rufe nichts hinter dem Block auf.
			}
			VarManager.deleteScope(this);
		} else if (nextElse != null && !Interpreter.execute(((Scope) nextElse).getStart()))
			return false;
		return Interpreter.execute(nextElse == null ? getEnd() : endOfConstruct());
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
}