package datatypes;

import java.util.Iterator;

import exceptions.runtime.CastingException;
import exceptions.runtime.ShouldBeNaturalNrException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.main.loops.ForEachLoop;
import expressions.normal.array.ArrayAccess;
import expressions.special.DataType;
import expressions.special.ValueHolder;

/**
 * <pre>
 * This is any value of the type Array.
 * 
 * -It gets defined as a {@link Literal}.
 */
public final class ArrayValue extends Value implements Iterable<Value> {

	/** Merges two existing Arrays */
	public static ArrayValue concat(ArrayValue a1, ArrayValue a2) {
		if (a1.type != a2.type)
			throw new UnexpectedTypeError("Only two arrays of the same type can be concatenated.");
		ValueHolder[] content = new ValueHolder[a1.length() + a2.length()];
		System.arraycopy(a1.raw(true), 0, content, 0, a1.length());
		System.arraycopy(a2.raw(true), 0, content, a1.length(), a2.length());
		return new ArrayValue(a1.type, content);
	}
	/** Multiplies an existing Array n times */
	public static ArrayValue multiply(ArrayValue a, int n, int executedInLine) {
		if (n < 0)
			throw new ShouldBeNaturalNrException(executedInLine, "Array cannot be multiplied with negative numbers.");
		final int orgL = a.length();
		ValueHolder[] content = new ValueHolder[orgL * n];
		for (int i = 0; i < n; i++)
			System.arraycopy(a, 0, content, i * orgL, orgL);
		return new ArrayValue(a.type, content);
	}

	private final ValueHolder[] container;

	private final DataType type;

	/** Build an ArrayValue from an existing, initialised one. */
	public ArrayValue(DataType t, ArrayValue a) {
		container = a.container;
		type = t;
	}

	/**
	 * Constructs an array based of a type an multiple parameters.
	 * 
	 * @param type       has to be an arraytype.
	 * @param preInit    is the list of parameters
	 * @param initialise is true if the array gets build at runtime.
	 */
	public ArrayValue(DataType type, ValueHolder[] container) {
		if (container == null)
			throw new AssertionError("Container cannot be null!");
		if (!DataType.isArrayType(type))
			throw new UnexpectedTypeError("Type has to be an arraytype. Was " + type);
		this.container = container;
		this.type = type;
	}

	// CASTING--------------------------------------------------

	@Override
	public BoolValue asBool() {
		return length() != 0 ? new BoolValue(true) : new BoolValue(false);
	}

	@Override
	public ArrayValue asBoolArray() throws CastingException {
		return asTypedArray(DataType.BOOL_ARRAY);
	}

	@Override
	public NumberValue asNumber() {
		return new NumberValue(length());
	}

	@Override
	public ArrayValue asNumberArray() throws CastingException {
		return asTypedArray(DataType.NUMBER_ARRAY);
	}

	@Override
	public TextValue asText() {
		int iMax = length() - 1;
		if (iMax == -1)
			return new TextValue("[]");
		StringBuilder b = new StringBuilder();
		b.append('[');
		for (int i = 0;; i++) {
			b.append(get(i));
			if (i == iMax)
				return new TextValue(b.append(']').toString());
			b.append(", ");
		}
	}

	@Override
	public ArrayValue asTextArray() throws CastingException {
		return asTypedArray(DataType.TEXT_ARRAY);
	}

	/**
	 * Lazily casts every value in this Array to the specified type.
	 */
	private ArrayValue asTypedArray(DataType t) {
		if (!DataType.isArrayType(type))
			throw new UnexpectedTypeError("Type has to be an arraytype. Was " + type);
		return new ArrayValue(t, this);
	}

	@Override
	public ArrayValue asVarArray() {
		return asTypedArray(DataType.VAR_ARRAY);
	}

	// Non-Static Methods-----------------------------------------------------------

	@Override
	public boolean canCastTo(DataType type) {
		return switch (type) {
		case VAR_ARRAY -> true; // Gibt sich selbst zurück
		case VAR -> true; // Gibt sich selbst zurück
		case BOOL -> true; // IsEmpty
		case NUMBER -> true; // Gibt Länge zurück
		case TEXT_ARRAY -> true; // Gibt text-repräsentation zurück
		case TEXT -> true; // Gibt text-repräsentation zurück
		case BOOL_ARRAY -> type == DataType.BOOL_ARRAY; // Nur wenn es ein boolarray ist.
		case NUMBER_ARRAY -> type == DataType.NUMBER_ARRAY; // Nur wenn es ein numberarray ist.
		};
	}

	/**
	 * Returns a single value from this array.
	 * 
	 * @see {@link ArrayAccess#getValue()}
	 */
	public Value get(int i) {
		return switch (type) {
		case BOOL_ARRAY -> container[i].getValue().as(DataType.BOOL);
		case NUMBER_ARRAY -> container[i].getValue().as(DataType.NUMBER);
		case TEXT_ARRAY -> container[i].getValue().as(DataType.TEXT);
		case VAR_ARRAY -> container[i].getValue();
		default -> throw new UnexpectedTypeError("Unexpected type: " + type);
		};
	}

	@Override
	public DataType getType() {
		return type;
	}

	/**
	 * Returns this/the whole array.
	 */
	@Override
	public ArrayValue getValue() {
		return this;
	}

	/** Gets primarily used by the {@link ForEachLoop}. */
	@Override
	public Iterator<Value> iterator() {
		return new Iterator<Value>() {
			int i = 0;

			@Override
			public boolean hasNext() {
				return i < container.length;
			}

			@Override
			public Value next() {
				return get(i++);
			}
		};
	}

	/**
	 * Simply returns the number of entries.
	 */
	public int length() {
		return container.length;
	}

	// Static Methods -------------------------------------------------------

	/**
	 * Extracts the content of this array.
	 * 
	 * Do not use this in an Operation!
	 * 
	 * @param shouldGetEvaluated is: True if a Value[]-Array should get evaluated.
	 *                           False if the the lazy ValueHolders should get
	 *                           returned.
	 */
	public ValueHolder[] raw(boolean shouldGetEvaluated) {
		if (!shouldGetEvaluated)
			return container;
		Value[] content = new Value[length()];
		for (int i = 0; i < length(); i++)
			content[i] = container[i].getValue();
		return content;
	}

	/**
	 * Changes a value in this array.
	 * 
	 * @param idx is the index of the change.
	 * @param val is the new value.
	 */
	public void set(int idx, Value val) {
		container[idx] = val;
	}

	/**
	 * Only for debugging.
	 * 
	 * Use "asText().rawString();" for textual representation.
	 */
	@Override
	public String toString() {
		return getType().toString();
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
			for (int i = 0; i < length(); i++) {
				try {
					if (Value.eq(this, a).not().raw())
						return false;
				} catch (UnexpectedTypeError e) {
					return false;
				}
			}
			return true;
		}
		throw new UnexpectedTypeError("Tried to compare " + this + " to " + v + ".");
	}
}
