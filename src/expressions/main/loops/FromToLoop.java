package expressions.main.loops;

import static helper.Output.print;

import datatypes.NumberValue;
import expressions.main.CloseBlock;
import expressions.normal.LoopConnector;
import expressions.normal.OpenBlock;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.Interpreter;
import interpreter.VarManager;
import parser.program.ExpressionType;

public class FromToLoop extends MainExpression implements Scope {

	private ValueHolder from = null;
	private ValueHolder to = null;
	private ValueHolder inc = null;
	private OpenBlock block = null;

	public FromToLoop(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME);
	}

	@Override
	public void build(Expression... args) {
		from = (ValueHolder) args[1];
		to = (ValueHolder) args[3];
		if (args.length > 4 && args[4] instanceof LoopConnector)
			inc = (ValueHolder) args[5];
		else
			inc = new NumberValue(1);
		if (args[args.length - 1] instanceof OpenBlock)
			block = (OpenBlock) args[args.length - 1];
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		if (!doExecuteNext)
			throw new AssertionError("A for-to-loop has to be able to call the next line.");
		final long f = from.getValue().asInt().rawInt();
		final long t = to.getValue().asInt().rawInt();
		final long i = inc.getValue().asInt().rawInt();
		print("Executing FromToLoop-Loop. (From " + f + " to " + t + ". Inc: " + i);
		for (long cnt = f; (f < t ? cnt < t : cnt > t); cnt = cnt + (f < t ? i : -i)) {
			VarManager.registerScope(this);
			VarManager.initCounter(this, cnt);
			if (!Interpreter.execute(line + 1, true)) {
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
		return block.getMatch() == null ? line + 2 : ((CloseBlock) block.getMatch()).line + 1;
	}

	@Override
	public String getScopeName() {
		return "fromto" + getStart() + "-" + getEnd();
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "from-to";
	}
}
