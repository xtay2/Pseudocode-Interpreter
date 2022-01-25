package expressions.main.loops;

import static helper.Output.print;
import static parsing.program.ExpressionType.NAME;

import datatypes.NumberValue;
import datatypes.Value;
import exceptions.parsing.IllegalCodeFormatException;
import expressions.normal.Expression;
import expressions.normal.Name;
import expressions.normal.Variable;
import expressions.normal.brackets.OpenScope;
import expressions.special.DataType;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import interpreter.Interpreter;
import interpreter.VarManager;

public class ForEachLoop extends Scope implements Loop {

	private ValueHolder array = null;
	private Name elementName = null;

	public ForEachLoop(int line) {
		super(line);
		setExpectedExpressions(NAME);
	}

	/** [NAME] [CONTAINER] [OPEN_SCOPE] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 3)
			throw new AssertionError("Merge on a for-each-loop has to contain three elements: element, container and opened scope.");
		elementName = (Name) e[0];
		VarManager.nameCheck(elementName.getName(), getOriginalLine());
		array = (ValueHolder) e[1];
		block = (OpenScope) e[2];
	}

	@Override
	public boolean execute(ValueHolder... params) {
		print("Executing For-Each-In-Loop.");
		NumberValue repetitions = new NumberValue(0);
		try {
			for (Value e : array.getValue().asVarArray()) { // Cast to Var-Array
				VarManager.registerScope(this);
				VarManager.registerVar(new Variable(lineIdentifier, DataType.VAR, elementName, e));
				VarManager.initCounter(this, repetitions, getOriginalLine());
				if (!Interpreter.execute(lineIdentifier + 1)) {
					VarManager.deleteScope(this);
					return false; // Wenn durch return im Block abgebrochen wurde rufe nichts dahinter auf.
				}
				repetitions = NumberValue.add(repetitions, new NumberValue(1));
				VarManager.deleteScope(this);
			}
		} catch (ClassCastException e) {
			throw new IllegalCodeFormatException(getOriginalLine(), "Cannot iterate over anything other than an array.");
		}
		return Interpreter.execute(getEnd());
	}

	@Override
	public String getScopeName() {
		return "foreach" + getStart() + "-" + getEnd();
	}
}
