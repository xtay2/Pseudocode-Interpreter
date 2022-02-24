package datatypes;

import static types.specific.DataType.BOOL_ARRAY;
import static types.specific.DataType.INT_ARRAY;
import static types.specific.DataType.NUMBER_ARRAY;
import static types.specific.DataType.TEXT_ARRAY;
import static types.specific.DataType.VAR_ARRAY;
import static types.specific.DataType.isArrayType;

import java.math.BigInteger;
import java.util.Arrays;

import datatypes.numerical.IntValue;
import datatypes.numerical.NumberValue;
import exceptions.runtime.ShouldBeNaturalNrException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.containers.ArrayAccess;
import types.specific.DataType;

/**
 * <pre>
 * This is any value of the type Array.
 * 
 * -It gets defined as a {@link Literal}.
 */
public final class ArrayValue extends Value implements MergedExpression {

	private ValueHolder[] container;

	public ArrayValue(DataType type) {
		super(type);
		if (!isArrayType(type))
			throw new AssertionError("Type has to be an arraytype, was: " + type);
	}

	/** Input the container-array. */
	@Override
	public void merge(Expression... e) {
		container = Arrays.copyOf(e, e.length, ValueHolder[].class);
	}

	// CASTING--------------------------------------------------

	/** Acts as a isEmpty-Function */
	@Override
	public BoolValue asBool() {
		return BoolValue.valueOf(length() != 0);
	}

	@Override
	public TextValue asText() {
		int iMax = length() - 1;
		if (iMax == -1)
			return new TextValue("[]");
		StringBuilder b = new StringBuilder();
		b.append('[');
		for (int i = 0;; i++) {
			b.append(get(i).asText().value);
			if (i == iMax)
				return new TextValue(b.append(']').toString());
			b.append(", ");
		}
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
		if (!isArrayType(t))
			throw new UnexpectedTypeError("Type has to be an arraytype. Was " + type);
		ArrayValue arr = new ArrayValue(t);
		arr.merge(Arrays.copyOf(container, container.length, Expression[].class));
		return arr;
	}

	// Non-Static Methods-----------------------------------------------------------

	@Override
	public boolean canCastTo(DataType type) {
		return switch (type) {
			case VAR, VAR_ARRAY -> true; // Gibt sich selbst zurück
			case BOOL -> true; // IsEmpty
			case NUMBER, INT -> true; // Gibt Länge zurück
			case TEXT_ARRAY -> true; // Gibt text-repräsentation zurück
			case TEXT -> true; // Gibt text-repräsentation zurück
			case BOOL_ARRAY -> type == BOOL_ARRAY; // Nur wenn es ein boolarray ist.
			case INT_ARRAY -> type == INT_ARRAY; // Nur wenn es ein intarray ist.
			case NUMBER_ARRAY -> type == NUMBER_ARRAY; // Nur wenn es ein numberarray ist.
			// Not supported
			case OBJECT, OBJECT_ARRAY -> false;
		};
	}

	/**
	 * Returns a single value from this array.
	 * 
	 * @see {@link ArrayAccess#getValue()}
	 */
	public Value get(int i) {
		return switch ((DataType) type) {
			case VAR_ARRAY -> container[i].getValue();
			case BOOL_ARRAY -> container[i].getValue().asBool();
			case NUMBER_ARRAY -> container[i].getValue().asNumber();
			case INT_ARRAY -> container[i].getValue().asInt();
			case TEXT_ARRAY -> container[i].getValue().asText();
			case VAR, BOOL, NUMBER, INT, TEXT, OBJECT, OBJECT_ARRAY -> throw new AssertionError("Unsupported Operation.");
		};
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
		container[idx] = val;
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
	 * @param shouldGetEvaluated is: True if a Value[]-Array should get evaluated. False if the the lazy
	 *                           ValueHolders should get returned.
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
		throw new UnexpectedTypeError("Tried to compare " + this + " to " + v + ".");
	}

	// Operations

	/** Merges this {@link ArrayValue} with another one. */
	public ArrayValue concat(ArrayValue a) {
		if (type != a.type)
			throw new UnexpectedTypeError("Only two arrays of the same type can be concatenated.");
		ValueHolder[] content = new ValueHolder[length() + a.length()];
		System.arraycopy(raw(true), 0, content, 0, length());
		System.arraycopy(a.raw(true), 0, content, length(), a.length());
		ArrayValue arr = new ArrayValue((DataType) type);
		arr.merge(Arrays.copyOf(content, content.length, Expression[].class));
		return arr;
	}

	/** Multiplies this {@link ArrayValue} n times */
	public ArrayValue multiply(int n, int executedInLine) {
		if (n < 0)
			throw new ShouldBeNaturalNrException(executedInLine, "Array cannot be multiplied with negative numbers.");
		final int orgL = length();
		ValueHolder[] content = new ValueHolder[orgL * n];
		for (int i = 0; i < n; i++)
			System.arraycopy(raw(true), 0, content, i * orgL, orgL);
		ArrayValue arr = new ArrayValue((DataType) type);
		arr.merge(Arrays.copyOf(content, content.length, Expression[].class));
		return arr;
	}

	/** Returns {@link BoolValue#TRUE} if this array contains the specified element. */
	public BoolValue contains(Value element) {
		for (int i = 0; i < length(); i++) {
			if (Value.eq(get(i), element).value)
				return BoolValue.TRUE;
		}
		return BoolValue.FALSE;
	}
}
