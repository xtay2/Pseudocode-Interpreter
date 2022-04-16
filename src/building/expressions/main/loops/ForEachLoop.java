package building.expressions.main.loops;

import static building.types.specific.datatypes.ArrayType.VAR_ARRAY;

import java.math.BigInteger;

import building.expressions.abstractions.ScopeHolder;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.expressions.normal.containers.Variable;
import building.types.specific.KeywordType;
import building.types.specific.datatypes.SingleType;
import runtime.datatypes.array.ArrayValue;
import runtime.datatypes.numerical.NumberValue;

public class ForEachLoop extends Loop {

	/** The {@link ValueHolder} that gets called at the start of every new iteration. */
	private final ValueHolder arrayHolder;

	/** The temporary value that gets reset for every iteration. */
	private ArrayValue array;

	/** The name of the running element. */
	private final Name elementName;

	/**
	 * Creates a {@link ForEachLoop}.
	 * 
	 * @param elementName is the {@link Name} of the running element. Shouldn't be null.
	 * @param arrayH      is the Wrapper for the {@link ArrayValue} that later gets iterated over.
	 *                    Shouldn't be null.
	 * @param os          is the {@link OpenBlock} of this {@link ScopeHolder}. Shouldn't be null.
	 */
	public ForEachLoop(int lineID, Name elementName, ValueHolder arrayH, OpenBlock os) {
		super(lineID, KeywordType.FOR, os);
		this.elementName = elementName;
		this.arrayHolder = arrayH;
		if (elementName == null || arrayHolder == null)
			throw new AssertionError("ElementName and ArrayHolder cannot be null.");
	}

	@Override
	protected void initLoop() {
		super.initLoop();
		array = (ArrayValue) arrayHolder.getValue().as(VAR_ARRAY);
	}

	@Override
	protected boolean doContinue(NumberValue iteration) {
		if (iteration.isGreaterEq(NumberValue.create(BigInteger.valueOf(array.length()))))
			return false;
		// Variable
		new Variable(lineIdentifier, getScope(), SingleType.VAR, elementName, array.get(iteration.asInt().value.intValueExact()));
		return true;
	}
}
