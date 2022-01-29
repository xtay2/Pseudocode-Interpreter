package datatypes;

import java.math.BigDecimal;

import exceptions.runtime.CastingException;
import exceptions.runtime.ShouldBeNaturalNrException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.normal.Expression;
import expressions.special.DataType;
import expressions.special.ValueHolder;

public class TextValue extends Value {

	public static TextValue concat(TextValue t1, TextValue t2) {
		return new TextValue(t1.value + t2.value);
	}

	public static TextValue multiply(TextValue t, int times, int executedInLine) {
		if (times < 0)
			throw new ShouldBeNaturalNrException(executedInLine, "Text cannot be multiplied with negative numbers.");
		return new TextValue(t.value.repeat(times));
	}

	private final String value;

	public TextValue(char val) {
		this(String.valueOf(val));
	}

	public TextValue(String val) {
		value = val;
	}

	@Override
	public BoolValue asBool() throws CastingException {
		Boolean b = asBoolValue(value);
		if (b == null)
			throw new CastingException("Only boolean literals and 1 and 0 can be casted from text to bool.");
		return new BoolValue(b.booleanValue());
	}

	@Override
	public ArrayValue asBoolArray() throws CastingException {
		throw new CastingException("Text cannot be casted to an array.");
	}

	@Override
	public NumberValue asNumber() throws CastingException {
		if (Value.isNumber(value))
			return new NumberValue(new BigDecimal(value));
		throw new CastingException("Cannot cast values other than numbers or boolean literals from text to number.\n");
	}

	@Override
	public ArrayValue asNumberArray() throws CastingException {
		throw new CastingException("Text cannot be casted to a number-array.");
	}

	@Override
	public TextValue asText() throws CastingException {
		return this;
	}

	@Override
	public ArrayValue asTextArray() throws CastingException {
		ValueHolder[] chars = new ValueHolder[value.length()];
		for (int i = 0; i < value.length(); i++)
			chars[i] = (new TextValue(value.charAt(i)));
		ArrayValue arr = new ArrayValue(DataType.TEXT_ARRAY);
		arr.merge((Expression[]) chars);
		return arr;
	}

	@Override
	public ArrayValue asVarArray() throws CastingException {
		ValueHolder[] chars = new ValueHolder[value.length()];
		for (int i = 0; i < value.length(); i++)
			chars[i] = (new TextValue(value.charAt(i)));
		ArrayValue arr = new ArrayValue(DataType.TEXT_ARRAY);
		arr.merge((Expression[]) chars);
		return arr;
	}

	@Override
	public boolean canCastTo(DataType type) {
		return switch (type) {
		case VAR -> true; // Gibt sich selbst zur�ck
		case VAR_ARRAY -> true; // Gibt char-array zur�ck
		case TEXT_ARRAY -> true; // Gibt char-array zur�ck
		case TEXT -> true; // Gibt text-repr�sentation zur�ck
		case BOOL -> Value.asBoolValue(value) != null; // Nur wenn es tats�chlich ein Boolean literal ist.
		case NUMBER -> Value.isNumber(value); // Nur wenn es tats�chlich eine Zahl ist. Siehe: TextValue#asNumber
		// Not Supported
		case NUMBER_ARRAY, BOOL_ARRAY -> false;
		};
	}

	@Override
	public DataType getType() {
		return DataType.TEXT;
	}

	/**
	 * Returns the raw String value of this TextValue.
	 * 
	 * Do not use this in an Operation!
	 */
	public String rawString() {
		return value;
	}

	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
		if (v instanceof TextValue n)
			return value.equals(n.value);
		throw new UnexpectedTypeError("Tried to compare " + this + " to " + v + ".");
	}

	/**
	 * Checks if this TextValue contains a element.
	 */
	public BoolValue contains(Value element) {
		return new BoolValue(value.contains(element.asText().rawString()));
	}
}
