package expressions.main.loops;

import static helper.Output.print;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;

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
		setExpectedExpressions(LITERAL, NAME);
	}

	@Override
	public void merge(Expression... e) {
		if (e.length != 2)
			throw new AssertionError("Merge on a repeat-loop has to contain two elements: counter and opened scope.");
		counterInit = (ValueHolder) e[0];
		openScope = (OpenScope) e[1];
	}

	@Override
	public boolean execute(ValueHolder... params) {
		print("Executing Repeat-Statement.");
		NumberValue max = counterInit.getValue().asInt();
		if (NumberValue.isSmallerThan(max, new NumberValue(0)).raw())
			throw new DeclarationException(getOriginalLine(), "Count of repetitions must be positive.");
		for (NumberValue i = new NumberValue(0); NumberValue.isSmallerThan(i, max).raw(); NumberValue.add(i, new NumberValue(1))) {
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
