package datatypes;

import java.math.BigDecimal;
import java.math.BigInteger;

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
		String t = value.strip();
		if (!t.contains("/")) {
			if (t.equals(NumberValue.State.POS_INF.toString()))
				return NumberValue.POS_INF;
			if (t.equals(NumberValue.State.NEG_INF.toString()))
				return NumberValue.NEG_INF;
			if (t.matches("-?\\d+")) // Integer 1, -1, 10, 100, -100, 010
				return NumberValue.create(new BigInteger(t));
			if (t.matches("-?\\d+.\\d+")) // Decimal
				return NumberValue.create(new BigDecimal(t));
			if (t.matches("-?\\d+.\\d*\\(\\d+\\)")) { // Periodic to fraction
				int ps = t.indexOf('('), pe = t.indexOf(')'); // Periode start/end
				int d = ps - t.indexOf('.') - 1;
				int pLength = pe - ps - 1;
				boolean isNeg = t.startsWith("-");
				//@formatter:off
				NumberValue n = 
					NumberValue.add(
						NumberValue.create(new BigDecimal(t.substring(isNeg ? 1 : 0, ps))),
						NumberValue.mult(
							NumberValue.create(BigInteger.ONE, BigInteger.TEN.pow(d)),
							NumberValue.div(
								NumberValue.create(new BigInteger(t.substring(ps + 1, pe))),
								NumberValue.create(new BigInteger("9".repeat(pLength))))));
				//@formatter:on
				return isNeg ? NumberValue.signum(n) : n;
			}
		}
		// Fractions and NaN
		String[] parts = t.split("/");
		if (parts.length != 2)
			return NumberValue.NAN;
		return NumberValue.div(new TextValue(parts[0]).asNumber(), new TextValue(parts[1]).asNumber());
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
		return asTextArray().asVarArray();
	}

	@Override
	public boolean canCastTo(DataType type) {
		return switch (type) {
		case VAR -> true; // Gibt sich selbst zurück
		case VAR_ARRAY -> true; // Gibt char-array zurück
		case TEXT_ARRAY -> true; // Gibt char-array zurück
		case TEXT -> true; // Gibt text-repräsentation zurück
		case NUMBER -> true; // Wenn es eine Zahl ist, die Zahl, sonst NaN
		case BOOL -> Value.asBoolValue(value) != null; // Nur wenn es tatsächlich ein Boolean literal ist.
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
