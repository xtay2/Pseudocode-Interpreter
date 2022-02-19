package datatypes;

import static datatypes.NullValue.NULL;

import exceptions.runtime.CastingException;
import exceptions.runtime.UnexpectedTypeError;
import types.specific.DataType;

/**
 * @deprecated WIP: Don't use this, its only implemented here.
 */
public class ObjectValue extends Value {

	@Deprecated
	public ObjectValue() {
		super(DataType.OBJECT);
		throw new AssertionError("WIP: This isn't implemented, yet.");
	}

	@Override
	public BoolValue asBool() throws CastingException {
		return BoolValue.valueOf(this != NULL);
	}

	@Override
	public TextValue asText() throws CastingException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean canCastTo(DataType type) {
		return switch (type) {
		case VAR, OBJECT -> true; // Returns this
		case BOOL -> true; // Returns true if not null
		case NUMBER, INT -> true; // Returns NAN
		case TEXT, TEXT_ARRAY -> true; // Text or CharArray-Representation.
		// Not supported
		case VAR_ARRAY, BOOL_ARRAY, NUMBER_ARRAY, INT_ARRAY, OBJECT_ARRAY -> false;
		};
	}

	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
		// TODO Auto-generated method stub
		return false;
	}
}
