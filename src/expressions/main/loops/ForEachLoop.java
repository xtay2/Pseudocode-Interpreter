package expressions.main.loops;

import static helper.Output.print;

import datatypes.Value;
import exceptions.parsing.IllegalCodeFormatException;
import expressions.normal.LoopConnector;
import expressions.normal.Name;
import expressions.normal.Variable;
import expressions.normal.brackets.OpenBlock;
import expressions.special.Expression;
import expressions.special.Scope;
import expressions.special.Type;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.Interpreter;
import interpreter.VarManager;
import parsing.program.ExpressionType;

public class ForEachLoop extends Scope {

	private ValueHolder array = null;
	private Name elementName = null;

	public ForEachLoop(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.NAME);
	}

	@Override
	public void build(Expression... args) {
		if (!(args[1] instanceof Name) || !(args[2] instanceof LoopConnector) || !(args[3] instanceof ValueHolder))
			throw new IllegalCodeFormatException(getOriginalLine(), "Wrong structured for-each-in loop.");
		elementName = (Name) args[1];
		VarManager.nameCheck(elementName.getName(), getOriginalLine());
		array = (ValueHolder) args[3];
		block = (OpenBlock) args[4];
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Executing For-Each-In-Loop.");
		int repetitions = 0;
		if (!doExecuteNext)
			throw new AssertionError("A for-each-loop has to be able to call the next line.");
		try {
			for (Value e : array.getValue().asVarArray()) { // Cast to Var-Array
				VarManager.registerScope(this);
				new Variable(lineIdentifier, Type.VAR).initialise(elementName, e);
				VarManager.initCounter(this, repetitions, getOriginalLine());
				if (!Interpreter.execute(lineIdentifier + 1, true)) {
					VarManager.deleteScope(this);
					return false; // Wenn durch return im Block abgebrochen wurde rufe nichts dahinter auf.
				}
				repetitions++;
				VarManager.deleteScope(this);
			}
		} catch (ClassCastException e) {
			throw new IllegalCodeFormatException(getOriginalLine(), "Cannot iterate over anything other than an array.");
		}
		return Interpreter.execute(getEnd(), true);
	}

	@Override
	public String getScopeName() {
		return "foreach" + getStart() + "-" + getEnd();
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "for-each";
	}

}
