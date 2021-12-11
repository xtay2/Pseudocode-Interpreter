package datatypes;

import java.util.ArrayList;
import java.util.List;

import exceptions.CastingException;
import expressions.special.Type;
import expressions.special.Value;
import expressions.special.ValueHolder;

public class TextValue extends Castable {

	private final String value;

	public TextValue(char val) {
		value = String.valueOf(val);
	}

	public TextValue(String val) {
		value = val;
	}

	@Override
	public ArrayValue asVarArray() throws CastingException {
		List<ValueHolder> params = new ArrayList<>(value.length());
		for (char c : value.toCharArray())
			params.add(new TextValue(c));
		return new ArrayValue(Type.VAR_ARRAY, params, true);
	}

	@Override
	public ArrayValue asBoolArray() throws CastingException {
		List<ValueHolder> params = new ArrayList<>(value.length());
		for (char c : value.toCharArray())
			params.add(new TextValue(c).asBool());
		return new ArrayValue(Type.BOOL_ARRAY, params, true);
	}

	@Override
	public ArrayValue asTextArray() throws CastingException {
		List<ValueHolder> params = new ArrayList<>(value.length());
		for (char c : value.toCharArray())
			params.add(new TextValue(c));
		return new ArrayValue(Type.TEXT_ARRAY, params, true);
	}

	@Override
	public ArrayValue asNumberArray() throws CastingException {
		List<ValueHolder> params = new ArrayList<>(value.length());
		for (char c : value.toCharArray())
			params.add(new TextValue(c).asNumber());
		return new ArrayValue(Type.NUMBER_ARRAY, params, true);
	}

	@Override
	public BoolValue asBool() throws CastingException {
		if ("1".equals(value) || "1.0".equals(value) || "true".equals(value))
			return new BoolValue(true);
		else if ("0".equals(value) || "0.0".equals(value) || "false".equals(value))
			return new BoolValue(false);
		else
			throw new CastingException("Cannot cast values other than numbers or boolean literals from text to bool.");
	}

	@Override
	public NumberValue asNumber() throws CastingException {
		if (Value.isInteger(value))
			return new NumberValue(Integer.valueOf(value));
		else if (Value.isNumber(value))
			return new NumberValue(Double.valueOf(value));
		else if (Value.isBoolean(value))
			return new BoolValue("true".equals(value)).asNumber();
		else
			throw new CastingException("Cannot cast values other than numbers or boolean literals from text to number.");
	}

	@Override
	public TextValue asText() throws CastingException {
		return this;
	}

	@Override
	public Type getType() {
		return Type.TEXT;
	}

	@Override
	public BoolValue eq(Castable val) {
		return new BoolValue(val instanceof TextValue t && t.value.equals(value));
	}

	@Override
	public BoolValue neq(Castable val) {
		return new BoolValue(!(val instanceof TextValue t && t.value.equals(value)));
	}

	/**
	 * Returns the raw String value of this TextValue.
	 * 
	 * Do not use this in an Operation!
	 */
	public String rawString() {
		return value;
	}

	public static TextValue concat(TextValue t1, TextValue t2) {
		return new TextValue(t1.value + t2.value);
	}

	public static TextValue multiply(TextValue t, int times) {
		return new TextValue(t.value.repeat(times));
	}
	
	@Override
	public String toString() {
		return "\"" + value + "\"";
	}
}
