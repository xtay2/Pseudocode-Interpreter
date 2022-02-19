package expressions.normal;

import static types.SuperType.DATA_TYPE;
import static types.specific.BuilderType.CLOSE_BRACKET;

import static types.specific.ExpressionType.NAME;
import static types.specific.ExpressionType.OPEN_SCOPE;

import expressions.abstractions.Expression;
import types.specific.DataType;

public class ExpectedType extends Expression implements Comparable<ExpectedType> {

	public final DataType type;

	public ExpectedType(DataType type, int line) {
		super(line, DATA_TYPE, NAME, OPEN_SCOPE, CLOSE_BRACKET);
		this.type = type;
	}

	@Override
	public int compareTo(ExpectedType o) {
		return o.type.compareTo(type);
	}
}
