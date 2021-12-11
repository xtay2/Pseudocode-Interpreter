package datatypes;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import exceptions.CastingException;
import expressions.special.Type;
import expressions.special.ValueHolder;

public final class ArrayValue extends Castable {

	private final List<ValueHolder> preInit;
	private final Castable[] content;
	private final Type type;
	private boolean initialised = false;

	public ArrayValue(Type t, ArrayValue a) {
		preInit = a.preInit;
		content = a.content;
		type = t;
		init();
	}

	/**
	 * Constructs an array based of a type an multiple parameters.
	 * 
	 * @param type       has to be an arraytype.
	 * @param preInit    is the list of parameters
	 * @param initialise is true if the array gets build at runtime.
	 */
	public ArrayValue(Type type, List<ValueHolder> preInit, boolean initialise) {
		content = new Castable[preInit.size()];
		if (!Type.isArrayType(type))
			throw new IllegalArgumentException("Type has to be an arraytype. Was " + type);
		this.type = type;
		this.preInit = preInit;
		if (initialise)
			init();
	}

	@Override
	public ArrayValue asVarArray() {
		return this;
	}

	@Override
	public ArrayValue asBoolArray() throws CastingException {
		if (type == Type.BOOL_ARRAY)
			return this;
		return new ArrayValue(Type.BOOL_ARRAY, this);
	}

	@Override
	public ArrayValue asTextArray() throws CastingException {
		if (type == Type.TEXT_ARRAY)
			return this;
		return new ArrayValue(Type.TEXT_ARRAY, this);
	}

	@Override
	public ArrayValue asNumberArray() throws CastingException {
		if (type == Type.NUMBER_ARRAY)
			return this;
		return new ArrayValue(Type.NUMBER_ARRAY, this);
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
		init();
		int iMax = length() - 1;
		if (iMax == -1)
			return new TextValue("[]");
		StringBuilder b = new StringBuilder();
		b.append('[');
		for (int i = 0;; i++) {
			b.append(get(i).toString());
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
	public BoolValue eq(Castable val) {
		return new BoolValue(val instanceof ArrayValue t && Arrays.equals(t.content, content));
	}

	@Override
	public BoolValue neq(Castable val) {
		return new BoolValue(!(val instanceof ArrayValue t && Arrays.equals(t.content, content)));
	}

	public void init() {
		if (initialised)
			return;
		for (int i = 0; i < content.length; i++)
			content[i] = preInit.get(i).getValue();
		initialised = true;
	}

	public Castable get(int i) {
		if (!initialised)
			throw new IllegalStateException("Array isn't initialised!");
		return switch (type) {
		case BOOL_ARRAY -> content[i].getValue().as(Type.BOOL);
		case NUMBER_ARRAY -> content[i].getValue().as(Type.NUMBER);
		case TEXT_ARRAY -> content[i].getValue().as(Type.TEXT);
		case VAR_ARRAY -> content[i].getValue();
		default -> throw new IllegalArgumentException("Unexpected value: " + type);
		};
	}
	
	public void set(int idx, Castable var) {
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
			throw new IllegalArgumentException("Only two arrays of the same type can be concatenated.");
		ArrayList<ValueHolder> values = new ArrayList<>(Arrays.asList(a1));
		values.addAll(Arrays.asList(a2));
		return new ArrayValue(a1.type, values, true);
	}

	/** Multiplies an existing Array n times */
	public static ArrayValue multiply(ArrayValue a, int n) {
		if (n < 0)
			throw new IllegalArgumentException("Array cannot be multiplied with negative numbers.");
		ArrayList<ValueHolder> values = new ArrayList<>(a.length() * n);
		for (int i = 0; i < n; i++)
			values.addAll(List.of(a.content));
		return new ArrayValue(a.type, values, true);
	}

	@Override
	public String toString() {
		return String.valueOf(type);
	}

}
