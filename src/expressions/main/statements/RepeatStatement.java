package expressions.main.statements;

import static helper.Output.print;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;

import exceptions.runtime.DeclarationException;
import expressions.normal.brackets.OpenBlock;
import expressions.special.Expression;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.Interpreter;
import interpreter.VarManager;

public class RepeatStatement extends Scope {

	private ValueHolder counterInit = null;

	public RepeatStatement(int line) {
		super(line);
		setExpectedExpressions(LITERAL, NAME);
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
		long max = counterInit.getValue().asInt().rawInt();
		if (max < 0)
			throw new DeclarationException(getOriginalLine(), "Count of repetitions must be positive.");
		if (!doExecuteNext)
			throw new AssertionError("A repetion-statement has to be able to call the next line.");
		for (int i = 0; i < max; i++) {
			VarManager.registerScope(this);
			VarManager.initCounter(this, i, getOriginalLine());
			if (!Interpreter.execute(lineIdentifier + 1, true)) {
				VarManager.deleteScope(this);
				return false; // Wenn durch return im Block abgebrochen wurde rufe nichts dahinter auf.
			}
			VarManager.deleteScope(this);
		}
		return Interpreter.execute(getEnd(), true);
	}

	@Override
	public String getScopeName() {
		return "repeat" + getStart() + "-" + getEnd();
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "repeat";
	}

}
