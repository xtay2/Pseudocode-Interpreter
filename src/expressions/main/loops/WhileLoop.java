package expressions.main.loops;

import static helper.Output.print;

import expressions.normal.brackets.OpenBlock;
import expressions.special.Expression;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.Interpreter;
import interpreter.VarManager;
import parsing.program.ExpressionType;

public class WhileLoop extends Scope {

	private ValueHolder runCondition = null;

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
			throw new AssertionError("A while-loop has to be able to call the next line.");
		while(runCondition.getValue().asBool().rawBoolean()) {
			VarManager.registerScope(this);
			VarManager.initCounter(this, repetitions);
			if (!Interpreter.execute(line + 1, true)) {
				VarManager.deleteScope(this);
				return false; // Wenn durch return im Block abgebrochen wurde rufe nichts dahinter auf.
			}
			repetitions++;
			VarManager.deleteScope(this);
		}
		return Interpreter.execute(getEnd(), true);
	}

	@Override
	public String getScopeName() {
		return "while" + getStart() + "-" + getEnd();
	}
	
	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "while";
	}
}
