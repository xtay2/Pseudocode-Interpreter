package runtime.datatypes.array;

import static building.types.specific.datatypes.SingleType.VAR;
import static runtime.datatypes.MaybeValue.NULL;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import building.expressions.abstractions.Range;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.containers.ArrayAccess;
import building.expressions.normal.containers.Literal;
import building.types.specific.datatypes.DataType;
import errorhandeling.NonExpressionException;
import errorhandeling.PseudocodeException;
import importing.filedata.paths.DataPath;
import misc.constants.TypeConstants;
import misc.helper.CollectionHelper;
import misc.helper.MathHelper;
import runtime.datatypes.BoolValue;
import runtime.datatypes.MaybeValue;
import runtime.datatypes.Value;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.textual.TextValue;

/**
 * <pre>
 * This is any value of the type Array.
 *
 * -It gets defined as a {@link Literal}.
 */
public final class ArrayValue extends Value implements Iterable<Value> {

	private Value[] content;
	private final DataType rules;

	/**
	 * Creates a {@link TypeConstants#VAR_ARR} with the given content.
	 *
	 * @param content shouldn't be null.
	 */
	public static ArrayValue newInstance(Value... content) {
		try {
			return new ArrayValue(TypeConstants.VAR_ARR, content);
		} catch (NonExpressionException e) {
			throw new AssertionError("This should not get thrown", e);
		}
	}

	/**
	 * Initialises a new {@link ArrayValue}.
	 *
	 * @param rules is a {@link DataType} that cannot be null. {@link DataType#ranges} have to be bigger
	 * than 0.
	 * @param content is a non-null {@link Value}-array. This can be empty.
	 * @throws NonExpressionException -> Casting
	 */
	public ArrayValue(DataType rules, Value... content) throws NonExpressionException {
		super(rules.type);
		assert content != null : "Arraycontent cannot be null.";
		assert rules.ranges.length > 0 : "Has to contain dimensions.";
		this.rules = rules;
		// Bounds-Check
		Range r = getRanges()[0];
		if (r.lowerBound > content.length) {
			throw new ArrayIndexOutOfBoundsException(
					content.length + " is an invalid length. This array has a lower bound of " + r.lowerBound);
		}
		if (content.length > r.upperBound) {
			throw new ArrayIndexOutOfBoundsException(
					content.length + " is an invalid length. This array has a upper bound of " + r.upperBound);
		}
		// Rule-Check
		this.content = cast(content, rules);
	}

	/**
	 * Deep-Casts any array to a given {@link DataType}.
	 *
	 * @param vals non-null values of the array.
	 * @param target {@link DataType}
	 * @throws NonExpressionException if null is forbidden.
	 */
	private static Value[] cast(Value[] vals, DataType target) throws NonExpressionException {
		DataType subRules = new DataType(target.type, target.allowsNull, Arrays.copyOfRange(target.ranges, 1, target.ranges.length));
		for (int i = 0; i < vals.length; i++) {
			if (!target.allowsNull && vals[i] == NULL) {
				throw new NonExpressionException("Casting",
						"You cannot cast an array, that contains null values, to one, that doesn't allow them.");
			}
			vals[i] = vals[i].as(subRules);
		}
		return vals;
	}

	/** Returns the {@link DataType} of this {@link ArrayValue}. */
	public DataType getRules() {
		return rules;
	}

	public boolean allowsNull() {
		return rules.allowsNull;
	}

	public Range[] getRanges() {
		return rules.ranges;
	}

	// CASTING--------------------------------------------------

	@Override
	public Value as(DataType t) throws NonExpressionException {
		if (t.isArrayType())
			return new ArrayValue(t, content);
		return switch (t.type) {
			case VAR -> this;
			case BOOL -> BoolValue.valueOf(length() != 0); // isEmpty
			case NR, INT -> new IntValue(length()); // length
			case TEXT -> asText();
			default -> ValueHolder.throwCastingExc(this, t);
		};
	}

	/**
	 * Casts this {@link ArrayValue} to a text-array.
	 *
	 * @throws NonExpressionException
	 */
	@Override
	public TextValue asText() {
		StringBuilder b = new StringBuilder();
		b.append('[');
		for (int i = 0; i < length(); i++) {
			b.append(get(i).asText().raw());
			if (i == length() - 1)
				return new TextValue(b.append(']').toString());
			b.append(", ");
		}
		return new TextValue("[]");
	}

	/**
	 * Returns a single value from this array.
	 *
	 * @see {@link ArrayAccess#getValue()}
	 */
	public Value get(int i) {
		return content[i];
	}

	/**
	 * Sets a value in this array and checks, if it matches the expected type.
	 *
	 * @param val is the new value.
	 * @param varName is the {@link String}-name of the variable which the array is assigned to.
	 * @param orgLine is the original line of the array-access in code.
	 * @param idxs is the n-dimensional index of the change.
	 */
	public Value set(Value val, String varName, DataPath dataPath, ValueHolder... idxs) {
		try {
			DataType expType = new DataType(dataType, rules.allowsNull,
					Arrays.copyOfRange(getRanges(), Math.min(idxs.length, getRanges().length), getRanges().length));
			if (!val.matches(expType))
				throw new NonExpressionException("ArrayAccess", "Tried to set the value " + val + " into \"" + varName
						+ "\" when only values of type \"" + rules + "\" are allowed.");
			return set(val, Arrays.stream(idxs).map(t -> {
				try {
					return MathHelper.valToInt(t);
				} catch (Exception e) {
					throw new PseudocodeException(e, dataPath);
				}
			}).collect(Collectors.toList()));
		} catch (Exception e) {
			throw new PseudocodeException(e, dataPath);
		}
	}

	/**
	 * Changes a value in possibly multiple dimensions. (Unchecked)
	 *
	 * @throws NonExpressionException -> Casting
	 */
	private Value set(Value val, List<Integer> idxs) throws NonExpressionException {
		int idx = idxs.remove(0);
		if (idxs.isEmpty()) { // Change in this dim
			Value prev = content[idx];
			content[idx] = val.as(elemType());
			return prev;
		}
		// Change underlying value
		return ((ArrayValue) content[idx]).set(val, new ArrayList<>(idxs));
	}

	/** Returns the {@link DataType} the elements in this {@link ArrayValue} should have. */
	private DataType elemType() {
		return new DataType(dataType, rules.allowsNull, Arrays.copyOfRange(getRanges(), 1, getRanges().length));
	}

	/** Simply returns the number of entries. */
	public int length() {
		return content.length;
	}

	/**
	 * Returns true, if this {@link ArrayValue} can be casted to the passed {@link DataType} without
	 * forbidding existing {@link MaybeValue#NULL}-values, or limiting the current {@link #length()}.
	 */
	public boolean allowsLosslessCastingTo(DataType type) {
		if (getRanges().length == type.getDims() || type.type == VAR) {
			if (!dataType.is(type.type) && dataType != VAR)
				return false;
			DataType inner;
			if (type.getDims() == 0) { // Check for one-dimensional var-type
				if (type.type != VAR)
					return false;
				inner = new DataType(VAR, type.allowsNull);
			} else
				inner = new DataType(type.type, type.allowsNull, Arrays.copyOfRange(type.ranges, 1, type.getDims()));
			// Cancel when array is empty, but subarrays are expected
			if (content.length == 0 && inner.getDims() >= 1)
				return false;
			for (Value v : content) {
				if (!v.matches(inner))
					return false;
			}
			// Here, every elem in the array matches the passed type.
			return (type.type == VAR && type.getDims() == 0)
					|| (type.ranges[0].lowerBound <= length() && length() <= type.ranges[0].upperBound);
		}
		return false; // Gets returned if the dimensions don't match
	}

	/** Returns a copy of the elements in this {@link ArrayValue}. */
	@Override
	public Value[] raw() {
		return Arrays.copyOf(content, length());
	}

	/**
	 * Recursivly compares all values of this array and the specified one.
	 *
	 * @return false if there is even one slight difference.
	 */
	@Override
	public boolean valueCompare(Value v) {
		if (v instanceof ArrayValue a) {
			if (length() != a.length())
				return false;
			for (int i = 0; i < length(); i++) {
				if (!Value.eq(get(i), a.get(i)).value)
					return false;
			}
			return true;
		}
		return false;
	}

	@Override
	public String toString() {
		return asText().raw();
	}

	// Operations

	/** Merges this {@link ArrayValue} with another one. */
	public ArrayValue concat(ArrayValue a) {
		Value[] content = CollectionHelper.merge(raw(), a.raw());
		return ArrayValue.newInstance(content);
	}

	/** Multiplies this {@link ArrayValue} n times */
	public ArrayValue multiply(int n, DataPath dataPath) {
		if (n < 0)
			throw new PseudocodeException("ShouldBeNaturalNrException", "Array cannot be multiplied with negative numbers.", dataPath);
		try {
			final int orgL = length();
			// Multiply Content
			Value[] content = new Value[orgL * n];
			for (int i = 0; i < n; i++)
				System.arraycopy(raw(), 0, content, i * orgL, orgL);
			return new ArrayValue(rules, content);
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, dataPath);
		}
	}

	/** Returns {@link BoolValue#TRUE} if this array contains the specified element. */
	public BoolValue contains(Value element) {
		for (Value v : this) {
			if (Value.eq(v, element).value)
				return BoolValue.TRUE;
		}
		return BoolValue.FALSE;
	}

	/**
	 * Appends a value at the end of this {@link ArrayValue}.
	 *
	 * @throws NonExpressionException -> Casting
	 *
	 * @throws CastingException if the {@link DataType}s didn't match.
	 */
	public ArrayValue append(Value val) throws NonExpressionException {
		Value[] content = new Value[length() + 1];
		System.arraycopy(content, 0, content, 0, length());
		content[length()] = val.as(elemType());
		return new ArrayValue(rules, content);
	}

	/**
	 * Prepend a value at the front of this {@link ArrayValue}.
	 *
	 * @throws NonExpressionException -> Casting
	 *
	 * @throws CastingException if the {@link DataType}s didn't match.
	 */
	public ArrayValue prepend(Value val) throws NonExpressionException {
		Value[] content = new Value[length() + 1];
		System.arraycopy(content, 0, content, 1, length());
		content[0] = val.as(elemType());
		return new ArrayValue(rules, content);
	}

	@Override
	public Iterator<Value> iterator() {
		return new Iterator<Value>() {

			int i = 0;

			@Override
			public boolean hasNext() {
				return i < length();
			}

			@Override
			public Value next() {
				return get(i++);
			}
		};
	}
}