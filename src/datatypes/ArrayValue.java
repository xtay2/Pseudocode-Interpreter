package datatypes;

import static types.specific.data.ArrayType.*;

import java.math.BigInteger;
import java.util.Arrays;

import datatypes.numerical.IntValue;
import datatypes.numerical.NumberValue;
import exceptions.runtime.CastingException;
import exceptions.runtime.ShouldBeNaturalNrException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.containers.ArrayAccess;
import types.specific.data.ArrayType;
import types.specific.data.DataType;
import types.specific.data.ExpectedType;

/**
 * <pre>
 * This is any value of the type Array.
 * 
 * -It gets defined as a {@link Literal}.
 */
public final class ArrayValue extends Value implements MergedExpression {

	private ValueHolder[] container;

	public ArrayValue(ArrayType type) {
		super(type);
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
	private ArrayValue asTypedArray(ArrayType t) {
		ArrayValue arr = new ArrayValue(t);
		arr.merge(Arrays.copyOf(container, container.length, Expression[].class));
		return arr;
	}

	// Non-Static Methods-----------------------------------------------------------

	@Override
	public boolean canCastTo(DataType type) {
		return switch (type) {
			case VAR -> true; // Gibt sich selbst zurück
			case BOOL -> true; // IsEmpty
			case NUMBER, INT -> true; // Gibt Länge zurück
			case TEXT -> true; // Gibt text-repräsentation zurück
			// Not supported
			case OBJECT -> false;
		};
	}

	@Override
	public boolean canCastTo(ArrayType type) {
		return switch (type) {
			case VAR_ARRAY -> true; // Gibt sich selbst zurück
			case TEXT_ARRAY -> true; // Gibt text-repräsentation zurück
			case NUMBER_ARRAY, INT_ARRAY -> true; // Casted jedes Element zu einer Zahl oder NaN.
			case BOOL_ARRAY -> type == BOOL_ARRAY; // Nur wenn es ein boolarray ist.
			// Not supported
			case OBJECT_ARRAY -> false;
		};
	}

	/**
	 * Returns a single value from this array.
	 * 
	 * @see {@link ArrayAccess#getValue()}
	 */
	public Value get(int i) {
		return switch ((ArrayType) type) {
			case VAR_ARRAY -> container[i].getValue();
			case BOOL_ARRAY -> container[i].getValue().asBool();
			case NUMBER_ARRAY -> container[i].getValue().asNumber();
			case INT_ARRAY -> container[i].getValue().asInt();
			case TEXT_ARRAY -> container[i].getValue().asText();
			case OBJECT_ARRAY -> throw new UnsupportedOperationException("Unimplemented case: " + type);
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
	public ArrayValue concat(ArrayValue a, int executedInLine) {
		if (!this.canCastTo((ArrayType) a.type) || !a.canCastTo((ArrayType) this.type))
			throw new CastingException(executedInLine,
					"Only two arrays of the same type can be concatenated. Tried " + type + " and " + a.type);
		ValueHolder[] content = new ValueHolder[length() + a.length()];
		System.arraycopy(raw(true), 0, content, 0, length());
		System.arraycopy(a.raw(true), 0, content, length(), a.length());
		ArrayValue arr = new ArrayValue((ArrayType) type);
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
		ArrayValue arr = new ArrayValue((ArrayType) type);
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

	/**
	 * Appends a value at the end of this {@link ArrayValue}.
	 * 
	 * @throws CastingException if the {@link ExpectedType}s didn't match.
	 */
	public ArrayValue append(Value val, int executedInLine) throws CastingException {
		if (!val.canCastTo(((ArrayType) type).dataType))
			throw new CastingException(executedInLine, "Trying to append " + val + " to " + this + ".");
		// Create
		ArrayValue arr = new ArrayValue((ArrayType) type);
		ValueHolder[] content = new ValueHolder[length() + 1];
		// Insert
		System.arraycopy(container, 0, content, 0, length());
		content[length()] = val;
		arr.merge(Arrays.copyOf(content, content.length, Expression[].class));
		return arr;
	}

	/**
	 * Prepend a value at the front of this {@link ArrayValue}.
	 * 
	 * @throws CastingException if the {@link ExpectedType}s didn't match.
	 */
	public ArrayValue prepend(Value val, int executedInLine) throws CastingException {
		if (!val.canCastTo(((ArrayType) type).dataType))
			throw new CastingException(executedInLine, "Trying to prepend " + val + " to " + this + ".");
		// Create
		ArrayValue arr = new ArrayValue((ArrayType) type);
		ValueHolder[] content = new ValueHolder[length() + 1];
		// Insert
		System.arraycopy(container, 0, content, 1, length());
		content[0] = val;
		arr.merge(Arrays.copyOf(content, content.length, Expression[].class));
		return arr;
	}
}
