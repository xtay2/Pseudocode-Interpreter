package runtime.datatypes.array;

import static building.types.specific.datatypes.ArrayType.*;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.containers.ArrayAccess;
import building.expressions.possible.allocating.Allocating;
import building.types.specific.datatypes.ArrayType;
import building.types.specific.datatypes.DataType;
import building.types.specific.datatypes.SingleType;
import misc.helper.CollectionHelper;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.numerical.NumberValue;
import runtime.datatypes.textual.TextValue;
import runtime.exceptions.ArrayAccessException;
import runtime.exceptions.CastingException;
import runtime.exceptions.ShouldBeNaturalNrException;
import runtime.exceptions.UnexpectedTypeError;

/**
 * <pre>
 * This is any value of the type Array.
 * 
 * -It gets defined as a {@link Literal}.
 */
public final class ArrayValue extends Value implements Iterable<Value> {

	private Value[] container;
	private ValueHolder[] preInit;

	/** Strict Constructor. Should always get used for spontaneous cases. */
	public ArrayValue(ArrayType type, Value... container) {
		this(type, (ValueHolder[]) container);
		init();
	}

	/** Lazy Constructor that waits for an {@link Allocating} to initialise this. */
	public ArrayValue(ArrayType type, ValueHolder... preInit) {
		super(type);
		if (preInit == null)
			throw new AssertionError("Arraycontent cannot be null.");
		this.preInit = preInit;
	}

	/** This gets called by every {@link Allocating} Expression. */
	public void init() {
		if (preInit == null && container == null)
			throw new IllegalStateException("Array cannot be unitialised and initialised at the same time.");
		// Already initialised
		if (preInit == null)
			return;
		// Init
		container = new Value[preInit.length];
		for (int i = 0; i < preInit.length; i++)
			set(preInit[i].getValue(), i);
		preInit = null;
		ArrayType.isInDimensions(this, getType().lengths, 0);
	}

	// CASTING--------------------------------------------------

	@Override
	@Deprecated
	public Value as(DataType t) throws CastingException {
		if (t.equals(getType()))
			return this;
		if (t instanceof ArrayType at && at.dimensions > 1) {
			Value[] content = Arrays.stream(container).map(e -> e.as(new ArrayType(at.dataType, at.dimensions - 1))).toArray(Value[]::new);
			System.out.println(Arrays.toString(content));
			return new ArrayValue(at, content);
		}
		System.out.println("Caste " + this + " zu " + t);
		return super.as(t);
	}

	/** Acts as a isEmpty-Function */
	@Override
	public BoolValue asBool() {
		init();
		return BoolValue.valueOf(length() != 0);
	}

	@Override
	public TextValue asText() {
		init();
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
		init();
		return NumberValue.create(BigInteger.valueOf(length()));
	}

	/** Returns the length of this Array, wrapped in a NumberValue. */
	@Override
	public NumberValue asNumber() {
		return asInt();
	}

	@Override
	public ArrayValue asVarArray() {
		return asTypedArray(VAR_ARRAY);
	}

	@Override
	public ArrayValue asNumberArray() {
		return asTypedArray(NUMBER_ARRAY);
	}

	@Override
	public ArrayValue asBoolArray() {
		return asTypedArray(BOOL_ARRAY);
	}

	@Override
	public ArrayValue asTextArray() {
		return asTypedArray(TEXT_ARRAY);
	}

	@Override
	public ArrayValue asIntArray() {
		return asTypedArray(INT_ARRAY);
	}

	@Override
	public ArrayValue asCharArray() {
		return asTypedArray(CHAR_ARRAY);
	}

	/**
	 * Lazily casts every value in this Array to the specified type.
	 */
	private ArrayValue asTypedArray(ArrayType t) {
		init();
		return new ArrayValue(t, container);
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
		if (type.dimensions > 1) {
			return Arrays.stream(container)
					.allMatch(e -> e instanceof ArrayValue av && av.canCastTo(new ArrayType(type.dataType, type.dimensions - 1)));
		}
		if (type.equals(VAR_ARRAY) || type.equals(TEXT_ARRAY) || type.equals(NUMBER_ARRAY))
			return true;
		return everyElementIs(type.dataType);
	}

	/**
	 * Checks, if every element in this array can get casted to the passed type.
	 */
	private boolean everyElementIs(SingleType t) {
		return Arrays.stream(container).allMatch(v -> v.canCastTo(t));
	}

	/**
	 * Returns a single value from this array.
	 * 
	 * @see {@link ArrayAccess#getValue()}
	 */
	public Value get(int i) {
		return container[i];
	}

	/**
	 * Changes a value in this array.
	 * 
	 * @param val  is the new value.
	 * @param idxs is the n-dimensional index of the change.
	 */
	public Value set(Value val, List<ValueHolder> idxs) {
		if (val == null)
			throw new AssertionError("Value cannot be null.");
		if (!val.canCastTo(getType().dataType))
			throw new CastingException("Tried to insert a " + val.type + " into a " + type + ".");
		init();
		if (idxs.size() == 1)
			return set(val, saveToInt(idxs.get(0)));
		idxs.remove(0);
		if (container[saveToInt(idxs.get(0))] instanceof ArrayValue av) {
			return av.set(val, new ArrayList<>(idxs));
		}
		throw new ArrayAccessException(getOriginalLine(),
				"Something went wrong while setting the value of the (sub)-array " + asText().raw());
	}

	@Deprecated
	private Value set(Value val, int idx) {
		Value old = container[idx];
		if (getType().dimensions > 1)
			container[idx] = val.as(new ArrayType(getType().dataType, getType().dimensions - 1));
		else
			container[idx] = val.as(getType());
		return old;
	}

	private static int saveToInt(ValueHolder vh) {
		return vh.getValue().asInt().raw().intValueExact();
	}

	@Override
	public ArrayType getType() {
		return (ArrayType) super.getType();
	}

	/**
	 * Simply returns the number of entries.
	 */
	public int length() {
		return container.length;
	}

	@Override
	public Value[] raw() {
		return Arrays.copyOf(container, length());
	}

	/**
	 * Recursivly compares all values of this array and the specified one.
	 * 
	 * @return false if there is even one slight difference.
	 */
	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
		if (v instanceof ArrayValue a) {
			if (length() != a.length())
				return false;
			try {
				for (int i = 0; i < length(); i++) {
					if (!Value.eq(this, a).value)
						return false;
				}
			} catch (UnexpectedTypeError e) {
				return false;
			}
			return true;
		}
		throw new AssertionError("Tried to compare " + this + " to " + v + ".");
	}

	// Operations

	/** Merges this {@link ArrayValue} with another one. */
	public ArrayValue concat(ArrayValue a, int executedInLine) {
		init();
		a.init();
		if (!this.canCastTo(a.getType()) || !a.canCastTo(getType()))
			throw new CastingException(executedInLine,
					"Only two arrays of the same type can be concatenated. Tried " + type + " and " + a.type);
		Value[] content = CollectionHelper.merge(raw(), a.raw());
		return new ArrayValue(getType(), content);
	}

	/** Multiplies this {@link ArrayValue} n times */
	public ArrayValue multiply(int n, int executedInLine) {
		init();
		if (n < 0)
			throw new ShouldBeNaturalNrException(executedInLine, "Array cannot be multiplied with negative numbers.");
		final int orgL = length();
		// Multiply Content
		Value[] content = new Value[orgL * n];
		for (int i = 0; i < n; i++)
			System.arraycopy(raw(), 0, content, i * orgL, orgL);
		return new ArrayValue(getType(), content);
	}

	/**
	 * Returns {@link BoolValue#TRUE} if this array contains the specified element.
	 */
	public BoolValue contains(Value element) {
		init();
		for (int i = 0; i < length(); i++) {
			if (Value.eq(get(i), element).value)
				return BoolValue.TRUE;
		}
		return BoolValue.FALSE;
	}

	/**
	 * Appends a value at the end of this {@link ArrayValue}.
	 * 
	 * @throws CastingException if the {@link DataType}s didn't match.
	 */
	public ArrayValue append(Value val, int executedInLine) throws CastingException {
		init();
		if (!val.canCastTo(getType().dataType))
			throw new CastingException(executedInLine, "Trying to append " + val + " to " + this + ".");
		// Create
		Value[] content = new Value[length() + 1];
		// Insert
		System.arraycopy(container, 0, content, 0, length());
		content[length()] = val;
		return new ArrayValue(getType(), content);
	}

	/**
	 * Prepend a value at the front of this {@link ArrayValue}.
	 * 
	 * @throws CastingException if the {@link DataType}s didn't match.
	 */
	public ArrayValue prepend(Value val, int executedInLine) throws CastingException {
		init();
		if (!val.canCastTo(getType().dataType))
			throw new CastingException(executedInLine, "Trying to prepend " + val + " to " + this + ".");
		// Create
		Value[] content = new Value[length() + 1];
		// Insert
		System.arraycopy(container, 0, content, 1, length());
		content[0] = val;
		return new ArrayValue(getType(), content);
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
		String res = asText().raw();
		if (res.length() > 25)
			return res.substring(0, 20) + "...]";
		return res;
	}
}
