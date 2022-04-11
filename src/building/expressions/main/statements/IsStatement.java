package building.expressions.main.statements;

import static building.types.specific.KeywordType.IS;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.datatypes.DataType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;

/**
 * Nearly identical to instanceof in Java. Checks if a value is an instance of a given type.
 */
public class IsStatement extends Expression implements ValueHolder {

	private final ValueHolder val;
	private final DataType type;

	/**
	 * Creates an {@link IsStatement}.
	 * 
	 * @param val  shouldn't be null.
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
		return BoolValue.valueOf(val.getValue().is(type));
	}
}
