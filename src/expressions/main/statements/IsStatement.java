package expressions.main.statements;

import static types.SuperType.DATA_TYPE;

import datatypes.BoolValue;
import datatypes.Value;
import exceptions.parsing.IllegalCodeFormatException;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.ExpectedType;
import types.specific.DataType;
import types.specific.KeywordType;

/**
 * Nearly identical to instanceof in Java. Checks if a value is an instance of a given type.
 */
public class IsStatement extends Expression implements ValueHolder, MergedExpression, Statement {

	private DataType type;
	private ValueHolder val;

	public IsStatement(int line) {
		super(line, KeywordType.IS, DATA_TYPE);
	}

	@Override
	public void merge(Expression... e) {
		if (e.length != 2)
			throw new AssertionError("IsStatement has to be merged on two values.");
		val = (ValueHolder) e[0];
		type = ((ExpectedType) e[1]).type;
		if (type == DataType.VAR)
			throw new IllegalCodeFormatException(getOriginalLine(), "Var is no datatype that can be checked.");
	}

	@Override
	public Value getValue() {
		return BoolValue.valueOf(val.getValue().type == type);
	}
}
