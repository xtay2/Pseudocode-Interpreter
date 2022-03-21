package runtime.datatypes.array;

import static building.types.specific.DataType.*;

import java.math.BigInteger;
import java.util.Arrays;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.containers.ArrayAccess;
import building.expressions.possible.allocating.Allocating;
import building.types.specific.DataType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.numerical.NumberValue;
import runtime.datatypes.textual.TextValue;
import runtime.exceptions.CastingException;
import runtime.exceptions.ShouldBeNaturalNrException;
import runtime.exceptions.UnexpectedTypeError;

/**
 * <pre>
 * This is any value of the type Array.
 * 
 * -It gets defined as a {@link Literal}.
 */
public final class ArrayValue extends Value {

	private Value[] container;
	private ValueHolder[] preInit;

	/** Strict Constructor. Should always get used for spontaneous cases. */
	public ArrayValue(DataType type, Value... container) {
		this(type, (ValueHolder[]) container);
		init();
	}

	/** Lazy Constructor that waits for an {@link Allocating} to initialise this. */
	public ArrayValue(DataType type, ValueHolder... preInit) {
		super(type);
		if (!type.isArray())
			throw new UnexpectedTypeError(type);
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
			container[i] = preInit[i].getValue();
		preInit = null;
	}

	// CASTING--------------------------------------------------

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
			b.append(get(i).asText().value);
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

	/**
	 * Lazily casts every value in this Array to the specified type.
	 */
	private ArrayValue asTypedArray(DataType t) {
		init();
		if (t.isArray())
			return new ArrayValue(t, container);
		throw new UnexpectedTypeError(t);
	}

	// Non-Static Methods-----------------------------------------------------------

	@Override
	public boolean canCastTo(DataType type) {
		return switch (type) {
		case VAR -> true; // Gibt sich selbst zurück
		case BOOL -> true; // IsEmpty
		case NUMBER, INT -> true; // Gibt Länge zurück
		case TEXT -> true; // Gibt text-repräsentation zurück
		case VAR_ARRAY -> true; // Gibt sich selbst zurück
		case TEXT_ARRAY -> true; // Gibt text-repräsentation zurück
		case NUMBER_ARRAY, INT_ARRAY -> true; // Casted jedes Element zu einer Zahl oder NaN.
		case CHAR_ARRAY -> everyElementIs(CHAR_ARRAY);// Only if every element can be casted to a char.
		case BOOL_ARRAY -> everyElementIs(BOOL); // Only if every element can be casted to a bool.
		case DEF_ARRAY -> everyElementIs(DEF); // Only if every element can be casted to a def.
		default -> false;
		};
	}

	/**
	 * Checks, if every element in this array can get casted to the passed type.
	 */
	private boolean everyElementIs(DataType t) {
		for (Value v : container) {
			if (!v.canCastTo(t))
				return false;
		}
		return true;
	}

	/**
	 * Returns a single value from this array.
	 * 
	 * @see {@link ArrayAccess#getValue()}
	 */
	public Value get(int i) {
		return container[i].as(getType().toDataType());
	}

	/**
	 * Changes a value in this array.
	 * 
	 * @param idx is the index of the change.
	 * @param val is the new value.
	 */
	public void set(int idx, Value val) {
		if (val == null)
			throw new AssertionError("Value cannot be null.");
		if (!val.canCastTo(getType().toDataType()))
			throw new CastingException("Tried to insert a " + val.type + " into a " + type + ".");
		container[idx] = val;
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
		Value[] content = new Value[length() + a.length()];
		System.arraycopy(raw(), 0, content, 0, length());
		System.arraycopy(a.raw(), 0, content, length(), a.length());
		return new ArrayValue(getType(), content);
	}

	/** Multiplies this {@link ArrayValue} n times */
	public ArrayValue multiply(int n, int executedInLine) {
		init();
		if (n < 0)
			throw new ShouldBeNaturalNrException(executedInLine, "Array cannot be multiplied with negative numbers.");
		final int orgL = length();
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
		if (!val.canCastTo(getType().toDataType()))
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
		if (!val.canCastTo(getType().toDataType()))
			throw new CastingException(executedInLine, "Trying to prepend " + val + " to " + this + ".");
		// Create
		Value[] content = new Value[length() + 1];
		// Insert
		System.arraycopy(container, 0, content, 1, length());
		content[0] = val;
		return new ArrayValue(getType(), content);
	}
}
