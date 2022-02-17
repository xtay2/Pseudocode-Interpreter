package expressions.normal;

import static types.ExpressionType.NAME;
import static types.ExpressionType.OPEN_SCOPE;

import expressions.abstractions.Expression;
import types.ExpressionType;
import types.specific.DataType;

public class ExpectedType extends Expression implements Comparable<ExpectedType> {

	public final DataType type;

	public ExpectedType(DataType type, int line) {
		super(line, ExpressionType.DATA_TYPE, NAME, OPEN_SCOPE);
		this.type = type;
	}

	public ExpectedType(String type, int line) {
		this(DataType.stringToType(type), line);
	}

	@Override
	public int compareTo(ExpectedType o) {
		return o.type.compareTo(type);
	}
}
