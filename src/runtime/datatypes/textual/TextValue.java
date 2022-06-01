package runtime.datatypes.textual;

import static building.types.specific.datatypes.SingleType.TEXT;
import static runtime.datatypes.numerical.ConceptualNrValue.NAN;
import static runtime.datatypes.numerical.ConceptualNrValue.NEG_INF;
import static runtime.datatypes.numerical.ConceptualNrValue.POS_INF;

import java.math.BigDecimal;
import java.math.BigInteger;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.datatypes.DataType;
import errorhandeling.NonExpressionException;
import errorhandeling.PseudocodeException;
import importing.filedata.paths.DataPath;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.numerical.NumberValue;

public final class TextValue extends Value {

	public final String value;

	/** Creates a {@link TextValue} from a {@link Character}. */
	public TextValue(char val) {
		super(TEXT);
		value = String.valueOf(val);
	}

	/** Creates a {@link TextValue} from a {@link String}. */
	public TextValue(String val) {
		super(TEXT);
		value = val;
	}

	@Override
	public Value as(DataType t) throws NonExpressionException {
		if (t.isArrayType()) {
			TextValue[] charArray = new TextValue[value.length()];
			for (int i = 0; i < charArray.length; i++)
				charArray[i] = new TextValue(value.charAt(i));
			return new ArrayValue(t, charArray);
		}
		return switch (t.type) {
			case VAR, TEXT -> this;
			case BOOL -> asBool();
			case CHAR -> asChar();
			case INT -> asNr().asInt();
			case NR -> asNr();
			default -> ValueHolder.throwCastingExc(this, t);
		};
	}

	@Override
	public BoolValue asBool() throws NonExpressionException {
		if (BoolValue.TRUE.toString().equals(value))
			return BoolValue.valueOf(true);
		if (BoolValue.FALSE.toString().equals(value))
			return BoolValue.valueOf(false);
		throw new NonExpressionException("Casting", "Only \"true\" & \"false\" can be casted from text to bool. \nWas: \"" + raw() + "\"");
	}

	@Override
	public NumberValue asNr() throws NonExpressionException {
		String t = value.strip();
		if (!t.contains("/")) {
			if (t.equals(POS_INF.txt))
				return POS_INF;
			if (t.equals(NEG_INF.txt))
				return NEG_INF;
			if (t.matches("-?\\d+")) // Integer 1, -1, 10, 100, -100, 010
				return new IntValue(new BigInteger(t));
			if (t.matches("-?\\d+.\\d+")) // Decimal
				return NumberValue.create(new BigDecimal(t));
			if (t.matches("-?\\d+.\\d*\\(\\d+\\)")) { // Periodic to fraction
				int ps = t.indexOf('('), pe = t.indexOf(')'); // Periode start/end
				int d = ps - t.indexOf('.') - 1;
				int pLength = pe - ps - 1;
				boolean isNeg = t.startsWith("-");
				NumberValue n = NumberValue.create(new BigDecimal(t.substring(isNeg ? 1 : 0, ps)))
						.add(NumberValue.create(BigInteger.ONE, BigInteger.TEN.pow(d))
								.mult(new IntValue(new BigInteger(t.substring(ps + 1, pe))))
								.div(new IntValue(new BigInteger("9".repeat(pLength)))));

				return isNeg ? n.negate() : n;
			}
		}
		// Fractions and NaN
		String[] parts = t.split("/");
		if (parts.length != 2)
			return NAN;
		return new TextValue(parts[0]).asNr().div(new TextValue(parts[1]).asNr());
	}

	@Override
	public CharValue asChar() throws NonExpressionException {
		if (value.length() == 1)
			return new CharValue(value.charAt(0));
		throw new NonExpressionException("Casting", "The text \"" + (value.length() > 20 ? value.substring(0, 15) + "..." : value)
				+ "\" consists of multiple chars, and therefore cannot be casted to one.");
	}

	@Override
	public boolean valueCompare(Value v) {
		return v.raw().toString().equals(value);
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
	public TextValue multiply(int times, DataPath dataPath) {
		if (times < 0)
			throw new PseudocodeException("ShouldBeNaturalNrException", "Text cannot be multiplied with negative numbers.", dataPath);
		return new TextValue(value.repeat(times));
	}

	@Override
	public String raw() {
		return new String(value);
	}

	@Override
	public String toString() {
		return "\"" + raw() + "\"";
	}
}
