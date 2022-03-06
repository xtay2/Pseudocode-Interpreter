package runtime.datatypes.array;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.possible.allocating.Allocating;
import building.types.specific.data.ArrayType;
import interpreting.modules.merger.ValueMerger;
import runtime.datatypes.Value;

/**
 * The {@link Value} that gets created by {@link ValueMerger#buildArrayLiteral()} during the
 * merge-phase.
 * 
 * It can later get turned into a real {@link ArrayValue} by any {@link Allocating}-Expression.
 */
public final class UnitialisedArrayValue implements ValueHolder {

	private ValueHolder[] content;
	private final ArrayType type;

	public UnitialisedArrayValue(ArrayType type, ValueHolder... content) {
		if (type == null)
			throw new AssertionError("Type cannot be null. Use var[] instead.");
		this.type = type;
		if (content == null)
			throw new AssertionError("Content cannot be null. Use empty array instead.");
		this.content = content;
	}

	/**
	 * Returns a initialised {@link ArrayValue}.
	 */
	@Override
	public ArrayValue getValue() {
		if (content == null)
			throw new AssertionError("Array cannot be initialised twice.");
		Value[] array = new Value[content.length];
		System.arraycopy(content, 0, array, 0, array.length);
		content = null;
		return new ArrayValue(type, array);
	}

}
