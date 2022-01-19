package expressions.main.loops;

import static helper.Output.print;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;

import expressions.normal.brackets.OpenBlock;
import expressions.special.Expression;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.Interpreter;
import interpreter.VarManager;

public class WhileUntilLoop extends Scope {

	public enum Type {
		UNTIL, WHILE;
	}

	private ValueHolder condition = null;

	private final Type type;

	public WhileUntilLoop(Type type, int line) {
		super(line);
		setExpectedExpressions(LITERAL, NAME);
		this.type = type;
	}

	@Override
	public void build(Expression... args) {
		condition = (ValueHolder) args[1];
		if (args[args.length - 1] instanceof OpenBlock)
			block = (OpenBlock) args[args.length - 1];
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Executing " + type + "-loop.");
		int repetitions = 0;
		if (!doExecuteNext)
			throw new AssertionError("A " + type + "-loop has to be able to call the next line.");
		while (condition.getValue().asBool().raw() == (type == Type.WHILE)) {
			VarManager.registerScope(this);
			VarManager.initCounter(this, repetitions, getOriginalLine());
			if (!Interpreter.execute(lineIdentifier + 1, true)) {
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
