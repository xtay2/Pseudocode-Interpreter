package datatypes;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import exceptions.runtime.CastingException;
import exceptions.runtime.DeclarationException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.special.Type;
import expressions.special.ValueHolder;

public final class ArrayValue extends Value implements Iterable<Value> {

	private final List<ValueHolder> preInit;
	private final Value[] content;
	private final Type type;
	private boolean initialised = false;

	/** Build an ArrayValue from an existing, initialised one. */
	public ArrayValue(Type t, ArrayValue a) {
		if (!a.initialised)
			throw new CastingException("Array has to be initialised!");
		preInit = null;
		initialised = true;
		content = a.content;
		type = t;
	}

	/**
	 * Constructs an array based of a type an multiple parameters.
	 * 
	 * @param type       has to be an arraytype.
	 * @param preInit    is the list of parameters
	 * @param initialise is true if the array gets build at runtime.
	 */
	public ArrayValue(Type type, List<ValueHolder> preInit) {
		if (preInit == null)
			throw new AssertionError("PreInit cannot be null!");
		if (!Type.isArrayType(type))
			throw new UnexpectedTypeError("Type has to be an arraytype. Was " + type);
		content = new Value[preInit.size()];
		this.type = type;
		this.preInit = preInit;
	}

	private ArrayValue asTypedArray(Type t) {
		if (!Type.isArrayType(type))
			throw new UnexpectedTypeError("Type has to be an arraytype. Was " + type);
		init();
		return new ArrayValue(t, this);
	}

	@Override
	public ArrayValue getValue() {
		init();
		return this;
	}

	@Override
	public ArrayValue asVarArray() {
		return asTypedArray(Type.VAR_ARRAY);
	}

	@Override
	public ArrayValue asBoolArray() throws CastingException {
		return asTypedArray(Type.BOOL_ARRAY);
	}

	@Override
	public ArrayValue asTextArray() throws CastingException {
		return asTypedArray(Type.TEXT_ARRAY);
	}

	@Override
	public ArrayValue asNumberArray() throws CastingException {
		return asTypedArray(Type.NUMBER_ARRAY);
	}

	@Override
	public BoolValue asBool() {
		return content.length != 0 ? new BoolValue(true) : new BoolValue(false);
	}

	@Override
	public NumberValue asNumber() {
		return new NumberValue(content.length);
	}

	@Override
	public TextValue asText() {
		int iMax = length() - 1;
		if (iMax == -1)
			return new TextValue("[]");
		init();
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
	public Type getType() {
		return type;
	}

	@Override
	public BoolValue eq(Value val) {
		return new BoolValue(val instanceof ArrayValue t && Arrays.equals(t.content, content));
	}

	@Override
	public BoolValue neq(Value val) {
		return new BoolValue(!(val instanceof ArrayValue t && Arrays.equals(t.content, content)));
	}

	private void init() {
		if (initialised)
			return;
		for (int i = 0; i < content.length; i++)
			content[i] = preInit.get(i).getValue();
		initialised = true;
	}

	public Value get(int i) {
		if (!initialised)
			throw new DeclarationException("Array isn't initialised!");
		return switch (type) {
		case BOOL_ARRAY -> content[i].getValue().as(Type.BOOL);
		case NUMBER_ARRAY -> content[i].getValue().as(Type.NUMBER);
		case TEXT_ARRAY -> content[i].getValue().as(Type.TEXT);
		case VAR_ARRAY -> content[i].getValue();
		default -> throw new UnexpectedTypeError("Unexpected type: " + type);
		};
	}

	public void set(int idx, Value var) {
		if (!initialised)
			init();
		content[idx] = var;
	}

	public int length() {
		return content.length;
	}

	/**
	 * Returns the raw ValueHolder array of this ArrayValue.
	 * 
	 * Do not use this in an Operation!
	 */
	public ValueHolder[] rawArray() {
		return content;
	}

	// STATIC OPERATIONS

	/** Merges two existing Arrays */
	public static ArrayValue concat(ArrayValue a1, ArrayValue a2) {
		if (a1.type != a2.type)
			throw new UnexpectedTypeError("Only two arrays of the same type can be concatenated.");
		List<ValueHolder> content = new ArrayList<>(a1.length() + a2.length());
		content.addAll(List.of(a1.getValue().content));
		content.addAll(List.of(a2.getValue().content));
		return new ArrayValue(a1.type, content);
	}

	/** Multiplies an existing Array n times */
	public static ArrayValue multiply(ArrayValue a, int n) {
		if (n < 0)
			throw new UnexpectedTypeError("Array cannot be multiplied with negative numbers.");
		Value[] contents = a.getValue().content;
		ArrayList<ValueHolder> content = new ArrayList<>(a.length() * n);
		for (int mult = 0; mult < n; mult++)
			content.addAll(List.of(contents));
		return new ArrayValue(a.type, content);
	}

	@Override
	public String toString() {
		return initialised ? asText().rawString() : getType().toString();
	}

	/** Gets primarily used by the ForEachLoop.*/
	@Override
	public Iterator<Value> iterator() {
		init();
		return new Iterator<Value>() {

			int i = 0;

			@Override
			public boolean hasNext() {
				return i < content.length;
			}

			@Override
			public Value next() {
				return get(i++);
			}
		};
	}
}
