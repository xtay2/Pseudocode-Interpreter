package building.expressions.possible.allocating;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.containers.Name;
import building.expressions.normal.containers.Variable;
import building.types.abstractions.SuperType;
import building.types.specific.datatypes.ArrayType;
import building.types.specific.datatypes.DataType;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;

public class ArrayDeclaration extends Declaration {

	/** Declaration for an array
	 * 
	 * @param type any {@link SuperType#ARRAY_TYPE}.
	 * @param maxLengths can optionally be null if no bounds are given. Tells the dimensions and maximum
	 * bounds.
	 * @param target should be non null
	 * @param val should be non null */
	public ArrayDeclaration(int lineID, ArrayType type, Name target, ValueHolder val) {
		super(lineID, type, target, val);
	}

	@Override
	public Value getValue() {
		ArrayValue v = (ArrayValue) val.getValue();
		new Variable(lineIdentifier, getScope(), (DataType) type, target.getName(), v).addFlags(flags);
		return v;
	}
}
