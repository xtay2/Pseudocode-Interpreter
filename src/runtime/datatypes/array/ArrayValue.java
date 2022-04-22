package runtime.datatypes.array;

import static building.types.specific.datatypes.ArrayType.NUMBER_ARRAY;
import static building.types.specific.datatypes.ArrayType.TEXT_ARRAY;
import static building.types.specific.datatypes.ArrayType.VAR_ARRAY;
import static runtime.datatypes.MaybeValue.NULL;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import building.expressions.abstractions.Range;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.containers.ArrayAccess;
import building.expressions.possible.allocating.Allocating;
import building.types.specific.datatypes.ArrayType;
import building.types.specific.datatypes.DataType;
import building.types.specific.datatypes.SingleType;
import misc.helper.CollectionHelper;
import misc.helper.MathHelper;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.numerical.NumberValue;
import runtime.datatypes.textual.TextValue;
import runtime.exceptions.CastingException;
import runtime.exceptions.ComparisonException;
import runtime.exceptions.NullNotAllowedException;
import runtime.exceptions.ShouldBeNaturalNrException;
import runtime.exceptions.UnexpectedTypeError;

/**
 * <pre>
 * This is any value of the type Array.
 * 
 * -It gets defined as a {@link Literal}. */
public final class ArrayValue extends Value implements Iterable<Value> {

	private Value[] container;
	private ValueHolder[] preInit;
	public final boolean allowNull;

	/** Strict Constructor. Should always get used for spontaneous cases. */
	public ArrayValue(ArrayType type, boolean allowNull, Value... container) {
		this(type, allowNull, (ValueHolder[]) container);
		init();
	}

	/** Lazy Constructor that waits for an {@link Allocating} to initialise this. */
	public ArrayValue(ArrayType type, boolean allowNull, ValueHolder... preInit) {
		super(type);
		assert preInit != null : "Arraycontent cannot be null.";
		this.allowNull = allowNull;
		this.preInit = preInit;
	}

	/** This gets called by every {@link Allocating} Expression. */
	private void init() {
		if (preInit == null && container == null)
			throw new IllegalStateException("Array cannot be unitialised and initialised at the same time.");
		// Already initialised
		if (preInit == null)
			return;
		Range r = getType().ranges[0];
		if (r.lowerBound > preInit.length)
			throw new ArrayIndexOutOfBoundsException(
					preInit.length + " is an invalid length. This array has a lower bound of " + r.lowerBound);
		if (preInit.length > r.upperBound)
			throw new ArrayIndexOutOfBoundsException(
					preInit.length + " is an invalid length. This array has a upper bound of " + r.upperBound);
		// Init
		container = new Value[preInit.length];
		for (int i = 0; i < preInit.length; i++) {
			Value e = preInit[i].getValue();
			if (!allowNull && e instanceof ArrayValue av)
				e = av.unnullify();
			set(e, i);
		}
		preInit = null;
	}

	// CASTING--------------------------------------------------

	@Override
	public Value as(DataType t) {
		if (t instanceof SingleType st) {
			return switch (st) {
				case VAR -> this;
				case BOOL -> asBool();
				case CHAR -> asChar();
				case INT -> asInt();
				case NUMBER -> asNumber();
				case TEXT -> asText();
				default -> throw new UnexpectedTypeError(st);
			};
		}
		return asTypedArray((ArrayType) t);
	}

	/** Acts as a isEmpty-Function */
	@Override
	public BoolValue asBool() {
		return BoolValue.valueOf(length() != 0);
	}

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

	@Override
	public IntValue asInt() {
		return NumberValue.create(BigInteger.valueOf(length()));
	}

	/** Returns the length of this Array, wrapped in a NumberValue. */
	@Override
	public NumberValue asNumber() {
		return asInt();
	}

	/** Casts this array to the passed type. */
	public ArrayValue asTypedArray(ArrayType t) {
		return new ArrayValue(t, allowNull, container);
	}

	// Non-Static Methods-----------------------------------------------------------

	@Override
	public boolean canCastTo(SingleType type) {
		return switch (type) {
			case VAR -> true; // Gibt sich selbst zurück
			case BOOL -> true; // IsEmpty
			case NUMBER, INT -> true; // Gibt Länge zurück
			case TEXT -> true; // Gibt text-repräsentation zurück
			// Not supported
			case CHAR, OBJECT -> false;
		};
	}

	@Override
	public boolean canCastTo(ArrayType type) {
		if (type.equals(VAR_ARRAY) || type.equals(TEXT_ARRAY) || type.equals(NUMBER_ARRAY))
			return true;
		return everyElementIs(type.type);
	}

	/** Checks, if every element in this array can get casted to the passed type. */
	private boolean everyElementIs(SingleType t) {
		return Arrays.stream(container).allMatch(v -> v.canCastTo(t));
	}

	/** Returns a single value from this array.
	 * 
	 * @see {@link ArrayAccess#getValue()} */
	public Value get(int i) {
		return container[i];
	}

	/** Sets a value in this array and casts it to the expected type.
	 * 
	 * @param val is the new value.
	 * @param idxs is the n-dimensional index of the change. */
	public Value set(Value val, ValueHolder[] idxs) {
		return set(val, Arrays.stream(idxs).map(MathHelper::valToInt).collect(Collectors.toList()));
	}

	/** Changes a value in possibly multiple dimensions. */
	private Value set(Value val, List<Integer> idxs) {
		int idx = idxs.remove(0);
		if (idxs.isEmpty()) // Change in this dim
			return set(val, idx);
		// Change underlying value
		return ((ArrayValue) container[idx]).set(val, new ArrayList<>(idxs));
	}

	/** Sets a value in the first dimension and returns the previous value. */
	private Value set(Value val, int idx) {
		if (!allowNull && val == NULL)
			throw new NullNotAllowedException(-1, "This array doesn't accept null as content.");
		Value prev = container[idx];
		if (getType().getDims() == 1)
			container[idx] = val.as(getType().type);
		else {
			if (getType().ranges.length == 0)
				container[idx] = val;
			else
				container[idx] = val.as(new ArrayType(getType().type, Arrays.copyOfRange(getType().ranges, 1, getType().ranges.length)));
		}
		return prev;
	}

	@Override
	public ArrayType getType() {
		return (ArrayType) type;
	}

	/** Simply returns the number of entries. */
	public int length() {
		return container.length;
	}

	/** Returns a copy of the elements in this {@link ArrayValue}. */
	@Override
	public Value[] raw() {
		return Arrays.copyOf(container, length());
	}

	/** Recursivly compares all values of this array and the specified one.
	 * 
	 * @return false if there is even one slight difference. */
	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
		if (v instanceof ArrayValue a) {
			if (length() != a.length())
				return false;
			for (int i = 0; i < length(); i++) {
				if (!Value.eq(get(i), a.get(i)).value)
					return false;
			}
			return true;
		}
		throw new ComparisonException(this, v);
	}

	/** Initialises and returns this {@link ArrayValue}. */
	@Override
	public Value getValue() {
		init();
		return this;
	}

	// Operations

	/** Merges this {@link ArrayValue} with another one. */
	public ArrayValue concat(ArrayValue a, int executedInLine) {
		if (!this.canCastTo(a.getType()) || !a.canCastTo(getType()))
			throw new CastingException(executedInLine,
					"Only two arrays of the same type can be concatenated. Tried " + type + " and " + a.type);
		Value[] content = CollectionHelper.merge(raw(), a.raw());
		return new ArrayValue(getType(), allowNull && a.allowNull, content);
	}

	/** Multiplies this {@link ArrayValue} n times */
	public ArrayValue multiply(int n, int executedInLine) {
		if (n < 0)
			throw new ShouldBeNaturalNrException(executedInLine, "Array cannot be multiplied with negative numbers.");
		final int orgL = length();
		// Multiply Content
		Value[] content = new Value[orgL * n];
		for (int i = 0; i < n; i++)
			System.arraycopy(raw(), 0, content, i * orgL, orgL);
		return new ArrayValue(getType(), allowNull, content);
	}

	/** Returns {@link BoolValue#TRUE} if this array contains the specified element. */
	public BoolValue contains(Value element) {
		for (int i = 0; i < length(); i++) {
			if (Value.eq(get(i), element).value)
				return BoolValue.TRUE;
		}
		return BoolValue.FALSE;
	}

	/** Appends a value at the end of this {@link ArrayValue}.
	 * 
	 * @throws CastingException if the {@link DataType}s didn't match. */
	public ArrayValue append(Value val, int executedInLine) throws CastingException {
		if (!val.canCastTo(getType().type))
			throw new CastingException(executedInLine, "Trying to append " + val + " to " + this + ".");
		// Create
		Value[] content = new Value[length() + 1];
		// Insert
		System.arraycopy(container, 0, content, 0, length());
		content[length()] = val;
		return new ArrayValue(getType(), allowNull, content);
	}

	/** Prepend a value at the front of this {@link ArrayValue}.
	 * 
	 * @throws CastingException if the {@link DataType}s didn't match. */
	public ArrayValue prepend(Value val, int executedInLine) throws CastingException {
		if (!val.canCastTo(getType().type))
			throw new CastingException(executedInLine, "Trying to prepend " + val + " to " + this + ".");
		// Create
		Value[] content = new Value[length() + 1];
		// Insert
		System.arraycopy(container, 0, content, 1, length());
		content[0] = val;
		return new ArrayValue(getType(), allowNull, content);
	}

	/** Returns a copy of this array that allows null. */
	public ArrayValue nullify() {
		if (allowNull)
			return this;
		return new ArrayValue(getType(), true, container);
	}

	/** Returns a copy of this array that doesn't allow null. If one of the elements in this array is
	 * null, a {@link NullNotAllowedException} gets thrown. */
	public ArrayValue unnullify() {
		if (!allowNull)
			return this;
		return new ArrayValue(getType(), false, container);
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
				return get(i);
			}
		};
	}

	@Override
	public String toString() {
		return asText().raw();
	}
}
