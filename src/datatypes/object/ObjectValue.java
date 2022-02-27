package datatypes.object;

import static datatypes.object.NullValue.NULL;
import static types.specific.data.DataType.OBJECT;

import datatypes.BoolValue;
import datatypes.TextValue;
import datatypes.Value;
import exceptions.runtime.CastingException;
import exceptions.runtime.UnexpectedTypeError;
import types.specific.data.DataType;

/**
 * @deprecated WIP: Don't use this, its only implemented here.
 */
@Deprecated
public class ObjectValue extends Value {

	@Deprecated
	public ObjectValue() {
		super(OBJECT);
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
			case TEXT -> true; // Text or CharArray-Representation.
		};
	}

	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Object raw() {
		// TODO Auto-generated method stub
		return null;
	}
}
