package building.types.specific.datatypes;

import static building.types.specific.datatypes.SingleType.*;

import java.util.Arrays;

import building.types.abstractions.AbstractType;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;
import runtime.exceptions.InvalidDimensionException;

/**
 * Wrapper for {@link DataType} for array-instances.
 */
public final class ArrayType implements DataType {

	// Castable, one-dimensional Arrays @formatter:off
	public static final ArrayType 
			VAR_ARRAY    = new ArrayType(VAR, 1), 
			TEXT_ARRAY   = new ArrayType(TEXT, 1), 
			CHAR_ARRAY 	 = new ArrayType(CHAR, 1),
			NUMBER_ARRAY = new ArrayType(NUMBER, 1),
			INT_ARRAY    = new ArrayType(INT, 1),
			BOOL_ARRAY   = new ArrayType(BOOL, 1),
			OBJECT_ARRAY = new ArrayType(OBJECT, 1);
	// @formatter:on

	public final SingleType dataType;

	/**
	 * If {@link #lengths} is not null, this should allways be equivalent to lengths.length().
	 */
	public final int dimensions;

	/**
	 * Stores the maximum length for each dimension.
	 */
	public final int lengths[];

	public ArrayType(SingleType dataType, int[] lengths) {
		this(dataType, lengths.length, lengths);
	}

	public ArrayType(SingleType dataType, int dimensions) {
		this(dataType, dimensions, null);
	}

	private ArrayType(SingleType dataType, int dimensions, int[] lengths) {
		this.dataType = dataType;
		this.dimensions = dimensions;
		this.lengths = lengths;
		if (dimensions < 1 || dimensions > 8)
			throw new AssertionError("An array cannot have more than 8, or less then one dimension.");
	}

	@Override
	public AbstractType[] abstractExpected() {
		return null;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof ArrayType at)
			return dimensions == at.dimensions && dataType.equals(at.dataType);
		return false;
	}

	@Override
	public Value stdVal() {
		Value[] content = new Value[lengths == null ? 1 : lengths[lengths.length - 1]];
		Arrays.fill(content, dataType.stdVal());
		for (int i = dimensions - 2; i >= 0; i--) {
			Value[] temp = new Value[lengths == null ? 1 : lengths[i]];
			int[] subLengths = Arrays.copyOfRange(lengths, i + 1, lengths.length);
			for (int j = 0; j < temp.length; j++)
				temp[j] = new ArrayValue(new ArrayType(dataType, subLengths), Arrays.copyOf(content, content.length));
			content = temp;
		}
		return new ArrayValue(this, content);
	}

	/**
	 * Checks if an {@link ArrayValue} fits the specified bounds in every dimension.
	 * 
	 * @param a          is the {@link ArrayValue}.
	 * @param checkedDim is the currently dimension that gets checked at the moment (bc recursive). When
	 *                   called externally, 0 should get passed by default.
	 */
	public static void isInDimensions(ArrayValue a, int[] lengths, int checkedDim) {
		if (lengths == null)
			return;
		if (a.length() > lengths[checkedDim]) {
			throw new ArrayIndexOutOfBoundsException("The array " + a + " is too long. Its specified to have a length of "
					+ lengths[checkedDim] + (lengths.length > 1 ? " in dimension " + (checkedDim + 1) + "." : "."));
		}
		for (int i = 0; i < a.length(); i++) {
			Value e = a.get(i);
			if (e instanceof ArrayValue av) {
				isInDimensions(av, lengths, checkedDim + 1);
			} else if (checkedDim < lengths.length - 2) {
				throw new InvalidDimensionException(
						"The array" + a + " is expected to be " + lengths.length + "-dimensional,\nbut has an non-array element \"" + e
								+ "\" at index " + i + " in dimension " + (checkedDim + 1) + ".");
			}
		}
	}

	@Override
	public String toString() {
		return dataType + (lengths == null ? "[]".repeat(dimensions) : Arrays.toString(lengths));
	}
}
