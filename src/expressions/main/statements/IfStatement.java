package expressions.main.statements;

import static helper.Output.print;

import expressions.main.CloseBlock;
import expressions.normal.OpenBlock;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import interpreter.Interpreter;
import interpreter.VarManager;
import parser.program.ExpressionType;

public class IfStatement extends MainExpression implements Scope {

	private ValueHolder booleanExp = null;
	private OpenBlock block = null;

	public IfStatement(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME);
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
		if (booleanExp.getValue().asBool()) {
			VarManager.registerScope(this);
			if (!Interpreter.execute(line + 1, !isOneLineStatement())) {
				VarManager.deleteScope(this);
				return false; // Wenn durch return abgebrochen wurde, rufe nichts hinter dem Block auf.
			}
			VarManager.deleteScope(this);
		}
		return Interpreter.execute(getEnd(), true);
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

}