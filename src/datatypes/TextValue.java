package datatypes;

import static datatypes.numerical.ConceptualNrValue.NAN;
import static datatypes.numerical.ConceptualNrValue.NEG_INF;
import static datatypes.numerical.ConceptualNrValue.POS_INF;

import java.math.BigDecimal;
import java.math.BigInteger;

import datatypes.numerical.IntValue;
import datatypes.numerical.NumberValue;
import exceptions.runtime.CastingException;
import exceptions.runtime.ShouldBeNaturalNrException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueHolder;
import types.specific.DataType;

public final class TextValue extends Value {

	public final String value;

	/** Creates a {@link TextValue} from a {@link Character}. */
	public TextValue(char val) {
		this(String.valueOf(val));
	}

	/** Creates a {@link TextValue} from a {@link String}. */
	public TextValue(String val) {
		super(DataType.TEXT);
		value = val;
	}

	// Casting

	@Override
	public BoolValue asBool() throws CastingException {
		if ("true".equals(value))
			return BoolValue.valueOf(true);
		if ("false".equals(value))
			return BoolValue.valueOf(false);
		throw new CastingException("Only boolean literals and 1 and 0 can be casted from text to bool.");
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
		case VAR, TEXT -> true; // Returns this
		case VAR_ARRAY, TEXT_ARRAY -> true; // CharArray-Representation.
		case NUMBER, INT -> true; // The number or NAN if its just text.
		case BOOL -> value.equals("true") || value.equals("false"); // Nur wenn es tatsächlich ein Boolean literal ist.
		// Not Supported
		case NUMBER_ARRAY, INT_ARRAY, BOOL_ARRAY, OBJECT, OBJECT_ARRAY -> false;
		};
	}
	
	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
		if (v instanceof TextValue n)
			return value.equals(n.value);
		throw new UnexpectedTypeError("Tried to compare " + this + " to " + v + ".");
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
}
