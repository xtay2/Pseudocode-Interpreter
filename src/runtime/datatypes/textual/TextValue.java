package runtime.datatypes.textual;

import static building.types.specific.datatypes.ArrayType.TEXT_ARRAY;
import static building.types.specific.datatypes.SingleType.*;
import static runtime.datatypes.numerical.ConceptualNrValue.NAN;
import static runtime.datatypes.numerical.ConceptualNrValue.NEG_INF;
import static runtime.datatypes.numerical.ConceptualNrValue.POS_INF;

import java.math.BigDecimal;
import java.math.BigInteger;

import building.types.specific.datatypes.SingleType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.numerical.NumberValue;
import runtime.exceptions.CastingException;
import runtime.exceptions.ShouldBeNaturalNrException;

public final class TextValue extends Value {

	public final String value;

	/** Creates a {@link TextValue} from a {@link String}. */
	public TextValue(String val) {
		super(TEXT);
		value = val;
	}

	// Casting

	@Override
	public BoolValue asBool() throws CastingException {
		if ("true".equals(value))
			return BoolValue.valueOf(true);
		if ("false".equals(value))
			return BoolValue.valueOf(false);
		throw new CastingException("Only boolean literals and 1 and 0 can be casted from text to bool. \nWas: \"" + raw() + "\"");
	}

	@Override
	public NumberValue asNumber() throws CastingException {
		String t = value.strip();
		if (!t.contains("/")) {
			if (t.equals(POS_INF.txt))
				return POS_INF;
			if (t.equals(NEG_INF.txt))
				return NEG_INF;
			if (t.matches("-?\\d+")) // Integer 1, -1, 10, 100, -100, 010
				return NumberValue.create(new BigInteger(t));
			if (t.matches("-?\\d+.\\d+")) // Decimal
				return NumberValue.create(new BigDecimal(t));
			if (t.matches("-?\\d+.\\d*\\(\\d+\\)")) { // Periodic to fraction
				int ps = t.indexOf('('), pe = t.indexOf(')'); // Periode start/end
				int d = ps - t.indexOf('.') - 1;
				int pLength = pe - ps - 1;
				boolean isNeg = t.startsWith("-");
				NumberValue n = NumberValue.create(new BigDecimal(t.substring(isNeg ? 1 : 0, ps)))
						.add(NumberValue.create(BigInteger.ONE, BigInteger.TEN.pow(d))
								.mult(NumberValue.create(new BigInteger(t.substring(ps + 1, pe))))
								.div(NumberValue.create(new BigInteger("9".repeat(pLength)))));

				return isNeg ? n.negate() : n;
			}
		}
		// Fractions and NaN
		String[] parts = t.split("/");
		if (parts.length != 2)
			return NAN;
		return new TextValue(parts[0]).asNumber().div(new TextValue(parts[1]).asNumber());
	}

	@Override
	public IntValue asInt() {
		return asNumber().asInt();
	}

	@Override
	public TextValue asText() {
		return this;
	}

	@Override
	public CharValue asChar() throws CastingException {
		if (value.length() == 1)
			return new CharValue(value.charAt(0));
		throw new CastingException("The text \"" + (value.length() > 20 ? value.substring(0, 15) + "..." : value)
				+ "\" consists of multiple chars, and therefore cannot be casted to one.");
	}

	@Override
	public ArrayValue asCharArray() {
		CharValue[] chars = new CharValue[value.length()];
		for (int i = 0; i < value.length(); i++)
			chars[i] = (new CharValue(value.charAt(i)));
		return new ArrayValue(TEXT_ARRAY, chars);
	}

	@Override
	public ArrayValue asVarArray() throws CastingException {
		return asCharArray().asVarArray();
	}

	@Override
	public boolean canCastTo(SingleType type) {
		return switch (type) {
			case VAR, TEXT -> true; // Returns this
			case NUMBER, INT -> true; // The number or NAN if its just text.
			case CHAR -> value.length() == 1; // Only if its just one character
			case BOOL -> value.equals("true") || value.equals("false"); // Only if its a boolean literal
			default -> false;
		};
	}

	@Override
	public boolean valueCompare(Value v) {
		if (v instanceof TextValue n)
			return value.equals(n.value);
		throw new AssertionError("Tried to compare " + this + " to " + v + ".");
	}

	// OPERATIONS

	/** Checks if this TextValue contains a element. */
	public BoolValue contains(Value element) {
		return BoolValue.valueOf(value.contains(element.asText().value));
	}

	/** Appends a second text after this one. */
	public TextValue concat(TextValue v) {
		return new TextValue(value + v.value);
	}

	/** Multiplies this text n times. */
	public TextValue multiply(int times, int executedInLine) {
		if (times < 0)
			throw new ShouldBeNaturalNrException(executedInLine, "Text cannot be multiplied with negative numbers.");
		return new TextValue(value.repeat(times));
	}

	@Override
	public String raw() {
		return new String(value);
	}
}
