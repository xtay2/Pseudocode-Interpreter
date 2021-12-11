package datatypes;

import exceptions.CastingException;
import expressions.special.Type;
import expressions.special.ValueHolder;

public abstract class Castable implements ValueHolder {

	public abstract ArrayValue asVarArray() throws CastingException;
	
	public abstract ArrayValue asBoolArray() throws CastingException;
	
	public abstract ArrayValue asTextArray() throws CastingException;
	
	public abstract ArrayValue asNumberArray() throws CastingException;
	
	public abstract BoolValue asBool() throws CastingException;
	
	public abstract NumberValue asNumber() throws CastingException;
	
	public abstract TextValue asText() throws CastingException;
	
	public abstract Type getType();
	
	public abstract BoolValue eq(Castable val);
	
	public abstract BoolValue neq(Castable value);
	
	public NumberValue asInt() {
		return asNumber().asInt();
	}
	
	public final Castable as(Type t) {
		return switch (t) {
		case BOOL -> asBool();
		case BOOL_ARRAY -> asBoolArray();
		case NUMBER -> asNumber();
		case NUMBER_ARRAY -> asNumberArray();
		case TEXT -> asText();
		case TEXT_ARRAY -> asTextArray();
		case VAR_ARRAY -> asVarArray();
		case VAR -> this;
		default -> throw new IllegalArgumentException("Unexpected value: " + t);
		};
	}
	
	@Override
	public final Castable getValue() {
		return this;
	}
	
	@Override
	public abstract String toString();
}
