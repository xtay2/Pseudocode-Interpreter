package expressions.main.loops;

import static types.specific.ExpressionType.NAME;
import static types.specific.data.DataType.VAR;

import java.math.BigInteger;

import datatypes.ArrayValue;
import datatypes.numerical.NumberValue;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.brackets.OpenScope;
import expressions.normal.containers.Name;
import expressions.normal.containers.Variable;
import types.specific.KeywordType;

public class ForEachLoop extends Loop {

	/** The {@link ValueHolder} that gets called at the start of ervy new iteration. */
	private ValueHolder arrayHolder = null;

	/** The temporary value that gets reset for every iteration. */
	private ArrayValue array;

	/** The name of the running element. */
	private Name elementName = null;

	public ForEachLoop(int lineID) {
		super(lineID, KeywordType.FOR, NAME);
	}

	/** [NAME] [CONTAINER] [OPEN_SCOPE] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 3)
			throw new AssertionError("Merge on a for-each-loop has to contain three elements: element, container and opened scope.");
		elementName = (Name) e[0];
		arrayHolder = (ValueHolder) e[1];
		initScope((OpenScope) e[2]);
	}

	@Override
	protected void initLoop() {
		super.initLoop();
		array = arrayHolder.getValue().asVarArray();
	}

	@Override
	protected boolean doContinue(NumberValue iteration) {
		if (iteration.isGreaterEq(NumberValue.create(BigInteger.valueOf(array.length()))))
			return false;
		// Variable
		Variable.quickCreate(lineIdentifier, getScope(), VAR, elementName, array.get(iteration.asInt().value.intValueExact()));
		return true;
	}
}
