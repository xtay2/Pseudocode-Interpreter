package expressions.main.functions;

import static helper.Output.print;

import expressions.normal.Name;
import expressions.normal.brackets.OpenBlock;
import expressions.special.Expression;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.Interpreter;
import interpreter.VarManager;
import parsing.program.ExpressionType;
import parsing.program.KeywordType;

public class MainFunction extends Function {

	public MainFunction(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.OPEN_BLOCK);
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
		Interpreter.execute(line + 1, true);
		VarManager.deleteScope(this);
		return false;
	}
	
	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "main";
	}
}
