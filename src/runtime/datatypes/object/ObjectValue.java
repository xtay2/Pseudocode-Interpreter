package runtime.datatypes.object;

import building.types.specific.datatypes.*;
import runtime.datatypes.*;

public class ObjectValue extends Value {
	
	public ObjectValue(SingleType dataType) {
		super(dataType);
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public boolean valueCompare(Value v) {
		// TODO Auto-generated method stub
		return false;
	}
	
	@Override
	public ObjectValue raw() {
		return this;
	}
	
}
