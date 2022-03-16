package runtime.datatypes.object;

import static building.types.specific.DataType.*;
import static runtime.datatypes.object.NullValue.NULL;

import building.expressions.abstractions.interfaces.NameHolder;
import building.expressions.normal.containers.Name;
import building.types.specific.DataType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;
import runtime.datatypes.textual.TextValue;
import runtime.exceptions.CastingException;
import runtime.exceptions.UnexpectedTypeError;

/**
 * @deprecated WIP: Don't use this, its only implemented here.
 */
@Deprecated
public class ObjectValue extends Value implements NameHolder {

	@Deprecated
	public ObjectValue() {
		super(OBJECT);
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
			default -> false;
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

	@Override
	public Name getName() {
		// TODO Auto-generated method stub
		return null;
	}
}
