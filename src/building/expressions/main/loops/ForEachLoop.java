package building.expressions.main.loops;

import building.expressions.abstractions.ScopeHolder;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.expressions.normal.containers.Variable;
import building.types.specific.KeywordType;
import misc.constants.TypeConstants;
import runtime.datatypes.array.ArrayValue;
import runtime.datatypes.numerical.NumberValue;

public class ForEachLoop extends Loop {

	/** The {@link ValueHolder} that gets called at the start of every new iteration. */
	private final ValueHolder arrayHolder;

	/** The temporary value that gets reset for every iteration. */
	private ArrayValue array;

	/** The name for the running element. */
	private Name elemName;

	/**
	 * Creates a {@link ForEachLoop}.
	 *
	 * @param elementName is the {@link Name} of the running element. Shouldn't be null.
	 * @param arrayH is the Wrapper for the {@link ArrayValue} that later gets iterated over. Shouldn't
	 * be null.
	 * @param os is the {@link OpenBlock} of this {@link ScopeHolder}. Shouldn't be null.
	 */
	public ForEachLoop(int lineID, Name elemName, ValueHolder arrayH, OpenBlock os) {
		super(lineID, KeywordType.FOR, null, os);
		assert elemName != null : "ElemName cannot be null.";
		assert arrayH != null : "ArrayHolder cannot be null.";
		this.elemName = elemName;
		this.arrayHolder = arrayH;
	}

	@Override
	protected void initLoop() {
		super.initLoop();
		array = (ArrayValue) arrayHolder.as(TypeConstants.VAR_ARR);
	}

	@Override
	@SuppressWarnings("unlikely-arg-type")
	protected boolean doContinue(NumberValue iteration) {
		if (iteration.equals(array.length()))
			return false;
		new Variable(lineIdentifier, getScope(), TypeConstants.VAR, elemName, array.get(iteration.asInt().value.intValueExact()));
		return true;
	}
}
