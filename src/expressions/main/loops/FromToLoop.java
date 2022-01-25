package expressions.main.loops;

import static datatypes.NumberValue.add;
import static datatypes.NumberValue.isSmallerEq;
import static datatypes.NumberValue.isSmallerThan;
import static datatypes.NumberValue.sub;
import static helper.Output.print;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;

import datatypes.NumberValue;
import expressions.normal.Expression;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import interpreter.Interpreter;
import interpreter.VarManager;


public class FromToLoop extends Scope implements Loop {

	private ValueHolder from;
	private ValueHolder to;
	private ValueHolder inc;

	public FromToLoop(int line) {
		super(line);
		setExpectedExpressions(LITERAL, NAME);
	}

	/** [FROM] [TO] [?INTERVALL]) */
	@Override
	public void merge(Expression... e) {
		from = (ValueHolder) e[0];
		to = (ValueHolder) e[1];
		inc = (ValueHolder) e[2];
	}

	@Override
	public boolean execute(ValueHolder... params) {
		final NumberValue f = from.getValue().asInt();
		final NumberValue t = to.getValue().asInt();
		final NumberValue i = inc.getValue().asInt();
		print("Executing FromToLoop-Loop. (From " + f + " to " + t + ". Inc: " + i);
		for (NumberValue cnt = f; // INIT
				(isSmallerThan(f, t).raw() ? isSmallerEq(cnt, t).raw() : !isSmallerThan(cnt, t).raw()); // LOOP CONDITION
				cnt = add(cnt, (isSmallerThan(f, t).raw() ? i : sub(new NumberValue(0), i)))) { // INCREMENT
			VarManager.registerScope(this);
			VarManager.initCounter(this, cnt, getOriginalLine());
			if (!Interpreter.execute(lineIdentifier + 1)) {
				VarManager.deleteScope(this);
				return false; // Wenn durch return im Block abgebrochen wurde rufe nichts dahinter auf.
			}
			VarManager.deleteScope(this);
		}
		return Interpreter.execute(getEnd());
	}

	@Override
	public String getScopeName() {
		return "fromto" + getStart() + "-" + getEnd();
	}
}
