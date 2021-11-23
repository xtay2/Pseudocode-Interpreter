package programreader.expressions.main.functions;

import programreader.expressions.normal.Name;
import programreader.expressions.normal.OpenBlock;
import programreader.expressions.special.Expression;
import programreader.expressions.special.ValueHolder;
import programreader.interpreter.Interpreter;
import programreader.interpreter.VarManager;
import programreader.program.ExpressionType;
import programreader.program.KeywordType;

import static helper.Output.*;

public class MainFunction extends Function {

	public MainFunction(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.OPEN_BLOCK, ExpressionType.ONE_LINE_STATEMENT);
	}

	@Override
	public void build(Expression... args) {
		name = new Name(KeywordType.MAIN.keyword, line);
		if (args[args.length - 1] instanceof OpenBlock)
			block = (OpenBlock) args[args.length - 1];
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Executing Main.");
		if (!doExecuteNext)
			throw new AssertionError("Main Function has to be allowed to execute.");
		VarManager.registerScope(this);
		Interpreter.execute(line + 1, !isOneLineStatement());
		VarManager.deleteScope(this);
		return false;
	}
}
