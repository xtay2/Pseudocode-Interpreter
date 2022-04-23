package building.types.specific.datatypes;

import static building.types.specific.datatypes.SingleType.*;

import java.util.Arrays;

import building.expressions.abstractions.Range;
import building.types.abstractions.AbstractType;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;

/** Wrapper for {@link DataType} for array-instances. */
public final class ArrayType implements DataType {

	// Castable, one-dimensional Arrays @formatter:off 
	public static final ArrayType 
			VAR_ARRAY    = new ArrayType(VAR, Range.UNBOUNDED), 
			TEXT_ARRAY   = new ArrayType(TEXT, Range.UNBOUNDED), 
			CHAR_ARRAY 	 = new ArrayType(CHAR, Range.UNBOUNDED),
			NUMBER_ARRAY = new ArrayType(NUMBER, Range.UNBOUNDED),
			INT_ARRAY    = new ArrayType(INT, Range.UNBOUNDED),
			BOOL_ARRAY   = new ArrayType(BOOL, Range.UNBOUNDED),
			OBJECT_ARRAY = new ArrayType(OBJECT, Range.UNBOUNDED);
	// @formatter:on

	/** The enforced type of the elements. */
	public final SingleType type;

	/** The enforced lengths of the elements. */
	public final Range[] ranges;

	public ArrayType(SingleType type, Range... ranges) {
		if (ranges.length == 0)
			throw new AssertionError("An array cannot be zero-dimensional.");
		this.type = type;
		this.ranges = ranges;
	}

	@Override
	public AbstractType[] abstractExpected() {
		return null;
	}

	@Override
	public boolean equals(Object obj) {
		return obj instanceof ArrayType at && type == at.type && Arrays.equals(ranges, at.ranges);
	}

	/** Return the amount of dimensions this {@link ArrayType} contains. */
	public int getDims() {
		return ranges.length;
	}

	@Override
	public ArrayValue stdVal(boolean allowsNull) {
		Value[] content = new Value[ranges[ranges.length - 1].lowerBound];
		if (content.length > 0)
			Arrays.fill(content, type.stdVal(allowsNull));
		for (int i = ranges.length - 2; i >= 0; i--) {
			Value[] temp = new Value[ranges[i].lowerBound];
			Range[] subLengths = Arrays.copyOfRange(ranges, i + 1, ranges.length);
			for (int j = 0; j < temp.length; j++)
				temp[j] = new ArrayValue(new ArrayType(type, subLengths), allowsNull, Arrays.copyOf(content, content.length));
			content = temp;
		}
		return new ArrayValue(this, allowsNull, content);
	}

	@Override
	public String toString() {
		String res = type.toString();
		for (int i = 0; i < ranges.length; i++)
			res += "[" + ranges[i] + "]";
		return res;
	}

}
