package expressions.main.functions;

import static helper.Output.print;

import expressions.normal.Name;
import expressions.normal.OpenBlock;
import expressions.special.Expression;
import expressions.special.ValueHolder;
import interpreter.Interpreter;
import interpreter.VarManager;
import parser.program.ExpressionType;
import parser.program.KeywordType;

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
