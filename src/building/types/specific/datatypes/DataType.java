package building.types.specific.datatypes;

import java.util.Arrays;

import building.expressions.abstractions.Range;
import errorhandeling.NonExpressionException;
import runtime.datatypes.MaybeValue;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;

/**
 * @see SingleType
 * @see ArrayType
 */
public class DataType {

	public final SingleType type;
	public final boolean allowsNull;
	public final Range[] ranges;

	/**
	 * Creates a {@link DataType} from the following components:
	 *
	 * @param type is the {@link SingleType}
	 * @param allowsNull tells, if this type allows {@link MaybeValue#NULL}
	 * @param range
	 */
	public DataType(SingleType type, boolean allowsNull, Range... range) {
		assert type != null : "The type of a datatype cannot be null!";
		assert range != null : "The range of a datatype can be empty but not null!";
		this.type = type;
		this.allowsNull = allowsNull;
		this.ranges = range;
	}

	/**
	 * Returns the value that a variable gets when it has none at the declaration.
	 *
	 * @throws NonExpressionException for when this type doesn't support a stdVal.
	 */
	public Value stdVal() throws NonExpressionException {
		if (isArrayType()) {
			Value[] content = new Value[ranges[ranges.length - 1].lowerBound];
			if (content.length > 0)
				Arrays.fill(content, type.stdVal(allowsNull));
			for (int i = ranges.length - 2; i >= 0; i--) {
				Value[] temp = new Value[ranges[i].lowerBound];
				Range[] subLengths = Arrays.copyOfRange(ranges, i + 1, ranges.length);
				for (int j = 0; j < temp.length; j++)
					temp[j] = new ArrayValue(new DataType(type, allowsNull, subLengths), Arrays.copyOf(content, content.length));
				content = temp;
			}
			return new ArrayValue(this, content);
		}
		return type.stdVal(allowsNull);
	}

	/** Returns true, if this is an arraytype. */
	public boolean isArrayType() {
		return ranges.length > 0;
	}

	/**
	 * Returns the amount of dimensions for this {@link DataType}. If this isn't an arraytype, 0 gets
	 * returned.
	 */
	public int getDims() {
		return ranges.length;
	}

	@Override
	public boolean equals(Object obj) {
		return obj instanceof DataType dt && type.is(dt.type) && (allowsNull == dt.allowsNull) && Arrays.equals(ranges, dt.ranges);
	}

	@Override
	public String toString() {
		String res = type + (allowsNull ? "?" : "");
		for (int i = 0; i < ranges.length; i++)
			res += "[" + ranges[i] + "]";
		return res;
	}
}
