package datatypes;

import exceptions.runtime.CastingException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.special.DataType;

@Deprecated
public class ObjectValue extends Value {

	@Deprecated
	public ObjectValue() {
		throw new AssertionError("This isn't implemented, yet.");
	}

	@Override
	public BoolValue asBool() throws CastingException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ArrayValue asBoolArray() throws CastingException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NumberValue asNumber() throws CastingException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ArrayValue asNumberArray() throws CastingException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public TextValue asText() throws CastingException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ArrayValue asTextArray() throws CastingException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ArrayValue asVarArray() throws CastingException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean canCastTo(DataType type) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public DataType getType() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
		// TODO Auto-generated method stub
		return false;
	}

}
