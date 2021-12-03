package expressions.main.loops;

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

public class WhileLoop extends MainExpression implements Scope {

	private ValueHolder runCondition = null;
	private OpenBlock block = null;

	public WhileLoop(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME);
	}

	@Override
	public void build(Expression... args) {
		runCondition = (ValueHolder) args[1];
		if (args[args.length - 1] instanceof OpenBlock)
			block = (OpenBlock) args[args.length - 1];
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Executing While-Loop.");
		int repetitions = 0;
		if (!doExecuteNext)
			throw new IllegalStateException("A while-loop has to be able to call the next line.");
		while(runCondition.getValue().asBool()) {
			VarManager.registerScope(this);
			VarManager.initCounter(this, repetitions);
			if (!Interpreter.execute(line + 1, !isOneLineStatement())) {
				VarManager.deleteScope(this);
				return false; // Wenn durch return im Block abgebrochen wurde rufe nichts dahinter auf.
			}
			repetitions++;
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
		return "while" + getStart() + "-" + getEnd();
	}

	@Override
	public boolean isOneLineStatement() {
		return block == null;
	}
}
