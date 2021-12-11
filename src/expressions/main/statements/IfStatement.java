package expressions.main.statements;

import static helper.Output.print;

import expressions.main.CloseBlock;
import expressions.normal.OpenBlock;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.ValueHolder;
import interpreter.Interpreter;
import interpreter.VarManager;
import parser.program.ExpressionType;

public class IfStatement extends MainExpression implements ElifConstruct {

	protected ValueHolder booleanExp = null;
	protected OpenBlock block = null;
	protected ElifConstruct nextElse = null;

	public IfStatement(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME, ExpressionType.ARRAY_START);
	}

	@Override
	public void build(Expression... args) {
		booleanExp = (ValueHolder) args[1];
		if (args[args.length - 1] instanceof OpenBlock)
			block = (OpenBlock) args[args.length - 1];
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Executing If-Statement.");
		if (!doExecuteNext)
			throw new IllegalStateException("An if-statement has to be able to call the next line.");
		if (booleanExp.getValue().asBool().rawBoolean()) {
			VarManager.registerScope(this);
			if (!Interpreter.execute(line + 1, !isOneLineStatement())) {
				VarManager.deleteScope(this);
				return false; // Wenn durch return abgebrochen wurde, rufe nichts hinter dem Block auf.
			}
			VarManager.deleteScope(this);
		} else if (nextElse != null && !Interpreter.execute(nextElse.getStart(), true))
			return false;
		return Interpreter.execute(nextElse == null ? getEnd() : endOfConstruct(), true);
	}

	@Override
	public int getStart() {
		return line;
	}

	@Override
	public int getEnd() {
		return isOneLineStatement() || block.getMatch() == null ? line + 2 : ((CloseBlock) block.getMatch()).line + 1;
	}

	@Override
	public String getScopeName() {
		return "if" + getStart() + "-" + getEnd();
	}

	@Override
	public boolean isOneLineStatement() {
		return block == null;
	}

	/** Initialises the following elif / else statement. */
	@Override
	public void setNextElse(ElifConstruct nextElse) {
		if (this.nextElse != null)
			throw new IllegalArgumentException("Trying to connect more than one else to this if.");
		this.nextElse = nextElse;
	}

	@Override
	public int endOfConstruct() {
		if (nextElse != null)
			return nextElse.endOfConstruct();
		return getEnd();
	}
}