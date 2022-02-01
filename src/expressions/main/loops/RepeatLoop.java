package expressions.main.loops;

import static helper.Output.print;
import static parsing.program.ExpressionType.*;

import datatypes.NumberValue;
import exceptions.runtime.DeclarationException;
import expressions.normal.Expression;
import expressions.normal.brackets.OpenScope;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import interpreter.Interpreter;
import interpreter.VarManager;

public class RepeatLoop extends Scope implements Loop {

	private ValueHolder counterInit = null;

	public RepeatLoop(int line) {
		super(line);
		setExpectedExpressions(LITERAL, NAME, OPEN_SCOPE);
	}

	@Override
	public void merge(Expression... e) {
		if (e.length != 2)
			throw new AssertionError("Merge on a repeat-loop has to contain two elements: counter and opened scope.");
		counterInit = e[0] == null ? NumberValue.POS_INF : (ValueHolder) e[0];
		openScope = (OpenScope) e[1];
	}

	@Override
	public boolean execute(ValueHolder... params) {
		print("Executing Repeat-Statement.");
		NumberValue max = counterInit.getValue().asInt();
		if (max.isSmallerThan(NumberValue.ONE))
			throw new DeclarationException(getOriginalLine(), "Count of repetitions must be positive.");
		for (NumberValue i = NumberValue.ZERO; i.isSmallerThan(max); i = NumberValue.add(i, NumberValue.ONE)) {
			VarManager.registerScope(this);
			VarManager.initCounter(this, i, getOriginalLine());
			if (!callNextLine()) {
				VarManager.deleteScope(this);
				return false; // Wenn durch return im Block abgebrochen wurde rufe nichts dahinter auf.
			}
			VarManager.deleteScope(this);
		}
		return Interpreter.execute(getEnd());
	}

	@Override
	public String getScopeName() {
		return "repeat" + getStart() + "-" + getEnd();
	}
}
