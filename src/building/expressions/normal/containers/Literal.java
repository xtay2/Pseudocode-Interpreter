package building.expressions.normal.containers;

import java.util.Arrays;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.DynamicType;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;

/**
 * Container for any {@link Value} and any uninitialised {@link ArrayValue}.
 */
public class Literal extends Expression implements ValueHolder {

	private final Value val;
	private final ValueHolder[] arrayPreInit;

	/**
	 * Constructs a wrapping {@link Literal} around a {@link Value}.
	 *
	 * @param val shouldn't be null.
	 */
	public Literal(int lineID, Value val) {
		super(lineID, DynamicType.LITERAL);
		assert val != null : "Value cannot be null.";
		this.val = val;
		arrayPreInit = null;
	}

	/**
	 * Constructs a wrapping {@link Literal} around an unitilialized {@link ArrayValue} that can later
	 * be obtained by calling {@link #getValue()}.
	 *
	 * @param arrayPreInit shouldn't be null.
	 */
	public Literal(int lineID, ValueHolder[] arrayPreInit) {
		super(lineID, DynamicType.LITERAL);
		assert arrayPreInit != null : "Array cannot be null.";
		val = null;
		this.arrayPreInit = arrayPreInit;
	}

	@Override
	public Value getValue() {
		if (val != null)
			return val;
		if (arrayPreInit != null)
			return ArrayValue.newInstance(Arrays.stream(arrayPreInit).map(e -> e.getValue()).toArray(Value[]::new));
		throw new AssertionError("This literal must be either an array or not. Is: " + this);
	}

	@Override
	public String toString() {
		return val != null ? val.toString() : Arrays.toString(arrayPreInit);
	}
}
