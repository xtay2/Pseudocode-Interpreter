package datatypes;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Iterator;

import exceptions.runtime.CastingException;
import exceptions.runtime.ShouldBeNaturalNrException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.abstractions.Expression;
import expressions.abstractions.MergedExpression;
import expressions.abstractions.ValueHolder;
import expressions.main.loops.ForEachLoop;
import expressions.normal.containers.ArrayAccess;
import expressions.special.DataType;

/**
 * <pre>
 * This is any value of the type Array.
 * 
 * -It gets defined as a {@link Literal}.
 */
public final class ArrayValue extends Value implements Iterable<Value>, MergedExpression {

	private ValueHolder[] container;

	private final DataType type;

	/** Build an ArrayValue from an existing, initialised one. */
	public ArrayValue(DataType t) {
		type = t;
	}

	@Override
	public void merge(Expression... e) {
		container = Arrays.copyOf(e, e.length, ValueHolder[].class);
	}

	// CASTING--------------------------------------------------

	/** Acts as a isEmpty-Function */
	@Override
	public BoolValue asBool() {
		return length() != 0 ? new BoolValue(true) : new BoolValue(false);
	}

	/** Returns the length of this Array, wrapped in a NumberValue. */
	@Override
	public NumberValue asNumber() {
		return NumberValue.create(new BigDecimal(length()));
	}
	
	@Override
	public ArrayValue asVarArray() {
		return asTypedArray(DataType.VAR_ARRAY);
	}

	@Override
	public ArrayValue asNumberArray() throws CastingException {
		return asTypedArray(DataType.NUMBER_ARRAY);
	}
	
	@Override
	public ArrayValue asBoolArray() throws CastingException {
		return asTypedArray(DataType.BOOL_ARRAY);
	}
	
	@Override
	public ArrayValue asTextArray() throws CastingException {
		return asTypedArray(DataType.TEXT_ARRAY);
	}

	@Override
	public TextValue asText() {
		int iMax = length() - 1;
		if (iMax == -1)
			return new TextValue("[]");
		StringBuilder b = new StringBuilder();
		b.append('[');
		for (int i = 0;; i++) {
			b.append(get(i).asText().rawString());
			if (i == iMax)
				return new TextValue(b.append(']').toString());
			b.append(", ");
		}
	}

	/**
	 * Lazily casts every value in this Array to the specified type.
	 */
	private ArrayValue asTypedArray(DataType t) {
		if (!DataType.isArrayType(type))
			throw new UnexpectedTypeError("Type has to be an arraytype. Was " + type);
		ArrayValue arr = new ArrayValue(t);
		arr.merge(Arrays.copyOf(container, container.length, Expression[].class));
		return arr;
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

	// STATIC METHODS
	// -------------------------------------------------------------------

	/** Merges two existing Arrays */
	public static ArrayValue concat(ArrayValue a1, ArrayValue a2) {
		if (a1.type != a2.type)
			throw new UnexpectedTypeError("Only two arrays of the same type can be concatenated.");
		ValueHolder[] content = new ValueHolder[a1.length() + a2.length()];
		System.arraycopy(a1.raw(true), 0, content, 0, a1.length());
		System.arraycopy(a2.raw(true), 0, content, a1.length(), a2.length());
		ArrayValue arr = new ArrayValue(a1.type);
		arr.merge((Expression[]) content);
		return arr;
	}

	/** Multiplies an existing Array n times */
	public static ArrayValue multiply(ArrayValue a, int n, int executedInLine) {
		if (n < 0)
			throw new ShouldBeNaturalNrException(executedInLine, "Array cannot be multiplied with negative numbers.");
		final int orgL = a.length();
		ValueHolder[] content = new ValueHolder[orgL * n];
		for (int i = 0; i < n; i++)
			System.arraycopy(a.raw(true), 0, content, i * orgL, orgL);
		ArrayValue arr = new ArrayValue(a.type);
		arr.merge(Arrays.copyOf(content, content.length, Expression[].class));
		return arr;
	}

	public BoolValue contains(Value element) {
		for (Value v : this) {
			if (Value.eq(v, element).raw())
				return new BoolValue(true);
		}
		return new BoolValue(false);
	}
}
