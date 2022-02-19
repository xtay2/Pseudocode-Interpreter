package expressions.main.loops;

import static datatypes.numerical.NumberValue.ZERO;
import static types.specific.ExpressionType.NAME;

import java.math.BigInteger;

import datatypes.ArrayValue;
import datatypes.Value;
import datatypes.numerical.NumberValue;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.brackets.OpenScope;
import expressions.normal.containers.Name;
import expressions.normal.containers.Variable;
import modules.interpreter.VarManager;
import types.specific.DataType;
import types.specific.KeywordType;

public class ForEachLoop extends Loop {

	/** The {@link ValueHolder} that gets called at the start of ervy new iteration. */
	private ValueHolder array = null;

	/** The temporary value that gets reset for every iteration. */
	private ArrayValue temp;

	/** The name of the running element. */
	private Name elementName = null;

	public ForEachLoop(int line) {
		super(line, KeywordType.FOR, NAME);
	}

	/** [NAME] [CONTAINER] [OPEN_SCOPE] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 3)
			throw new AssertionError("Merge on a for-each-loop has to contain three elements: element, container and opened scope.");
		elementName = (Name) e[0];
		array = (ValueHolder) e[1];
		initScope((OpenScope) e[2]);
	}

	@Override
	protected boolean doContinue(NumberValue iteration) {
		// Init
		if (iteration.equals(ZERO))
			temp = (ArrayValue) array.getValue();
		// Continuation
		if (iteration.isGreaterEq(NumberValue.create(BigInteger.valueOf(temp.length()))))
			return false;
		// Variable
		Value e = temp.get(iteration.asInt().value.intValueExact());
		Variable var = VarManager.get(elementName.getName(), getOriginalLine());
		if (var == null)
			var = Variable.quickCreate(lineIdentifier, DataType.VAR, elementName, null);
		var.setValue(e);
		return true;
	}
}
