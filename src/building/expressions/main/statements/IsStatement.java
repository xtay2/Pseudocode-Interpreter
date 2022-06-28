package building.expressions.main.statements;

import static building.types.specific.KeywordType.*;
import static runtime.datatypes.MaybeValue.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.types.specific.datatypes.*;
import runtime.datatypes.*;
import runtime.datatypes.array.*;

/**
 * Nearly identical to instanceof in Java. Checks if a value is an instance of a given type.
 */
public class IsStatement extends Expression implements ValueHolder {
	
	private final ValueHolder val;
	private final DataType type;
	
	/**
	 * Creates an {@link IsStatement}.
	 *
	 * @param val shouldn't be null.
	 * @param type shouldn't be null.
	 */
	public IsStatement(int lineID, ValueHolder val, DataType type) {
		super(lineID, IS);
		this.val = val;
		this.type = type;
		if (val == null || type == null)
			throw new AssertionError("Value or Type cannot be null.");
	}
	
	@Override
	public Value getValue() {
		Value v = val.getValue();
		DataType typeOfVal;
		if (v instanceof ArrayValue at)
			typeOfVal = new DataType(v.dataType, at.allowsNull(), at.getRanges());
		else
			typeOfVal = new DataType(v.dataType, v == NULL);
		return BoolValue.valueOf(type.equals(typeOfVal));
	}
}
