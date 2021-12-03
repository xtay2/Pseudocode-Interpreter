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

public class RepeatStatement extends MainExpression implements Scope {

	private ValueHolder counterInit = null;
	private OpenBlock block = null;

	public RepeatStatement(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME);
	}

	@Override
	public void build(Expression... args) {
		counterInit = (ValueHolder) args[1];
		if (args[args.length - 1] instanceof OpenBlock)
			block = (OpenBlock) args[args.length - 1];
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Executing Repeat-Statement.");
		int max = counterInit.getValue().asInt();
		if (max < 0)
			throw new IllegalArgumentException("Count of repetitions must be positive.");
		if (!doExecuteNext)
			throw new IllegalStateException("A repetion-statement has to be able to call the next line.");
		for (int i = 0; i < max; i++) {
			VarManager.registerScope(this);
			if (!Interpreter.execute(line + 1, !isOneLineStatement())) {
				VarManager.deleteScope(this);
				return false; // Wenn durch return im Block abgebrochen wurde rufe nichts dahinter auf.
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
		return "repeat" + getStart() + "-" + getEnd();
	}

	@Override
	public boolean isOneLineStatement() {
		return block == null;
	}

}
