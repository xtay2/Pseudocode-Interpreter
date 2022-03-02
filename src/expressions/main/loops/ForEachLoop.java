package expressions.main.loops;

import static types.specific.data.DataType.VAR;

import java.math.BigInteger;

import datatypes.ArrayValue;
import datatypes.numerical.NumberValue;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.brackets.OpenScope;
import expressions.normal.containers.Name;
import expressions.normal.containers.Variable;
import types.specific.KeywordType;

public class ForEachLoop extends Loop {

	/** The {@link ValueHolder} that gets called at the start of ervy new iteration. */
	private final ValueHolder arrayHolder;

	/** The temporary value that gets reset for every iteration. */
	private ArrayValue array;

	/** The name of the running element. */
	private final Name elementName;

	public ForEachLoop(int lineID, Name elementName, ValueHolder arrayH, OpenScope os) {
		super(lineID, KeywordType.FOR, os);
		this.elementName = elementName;
		this.arrayHolder = arrayH;
		if (elementName == null || arrayHolder == null)
			throw new AssertionError("ElementName and ArrayHolder cannot be null.");
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
