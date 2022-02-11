package expressions.main.loops;

import static helper.Output.print;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;

import datatypes.NumberValue;
import expressions.normal.Expression;
import expressions.normal.brackets.OpenScope;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import interpreter.Interpreter;
import interpreter.VarManager;

public class WhileUntilLoop extends Scope implements Loop {

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
	public void merge(Expression... e) {
		if (e.length != 2)
			throw new AssertionError("Merge on a while-/until-statement has to contain a condition and an opened scope.");
		condition = (ValueHolder) e[0];
		openScope = (OpenScope) e[1];
	}

	@Override
	public boolean execute(ValueHolder... params) {
		print("Executing " + type + "-loop.");
		NumberValue repetitions = NumberValue.ZERO;
		while (condition.getValue().asBool().raw() == (type == Type.WHILE)) {
			VarManager.registerScope(this);
			VarManager.initCounter(this, repetitions, getOriginalLine());
			if (!callNextLine()) {
				VarManager.deleteScope(this);
				return false; // Wenn durch return im Block abgebrochen wurde rufe nichts dahinter auf.
			}
			repetitions = NumberValue.add(repetitions, NumberValue.ONE);
			VarManager.deleteScope(this);
		}
		return Interpreter.execute(getEnd());
	}

	@Override
	public String getScopeName() {
		return "while" + getStart() + "-" + getEnd();
	}
}
