package expressions.main.statements;

import static types.specific.KeywordType.IS;

import datatypes.BoolValue;
import datatypes.Value;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueHolder;
import types.specific.data.ExpectedType;

/**
 * Nearly identical to instanceof in Java. Checks if a value is an instance of a given type.
 */
public class IsStatement extends Expression implements ValueHolder {

	private final ValueHolder val;
	private final ExpectedType type;

	/**
	 * Creates an {@link IsStatement}.
	 * 
	 * @param val  shouldn't be null.
	 * @param type shouldn't be null.
	 */
	public IsStatement(int lineID, ValueHolder val, ExpectedType type) {
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
