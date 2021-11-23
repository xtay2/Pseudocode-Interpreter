package programreader.expressions.normal;

import programreader.expressions.special.Expression;
import programreader.expressions.special.Type;
import programreader.program.ExpressionType;

public class ExpectedType extends Expression implements Comparable<ExpectedType> {

	public final Type type;

	public ExpectedType(String type, int line) {
		this(Type.stringToType(type), line);
	}

	public ExpectedType(Type type, int line) {
		super(line);
		setExpectedExpressions(ExpressionType.NAME, ExpressionType.OPEN_BLOCK, ExpressionType.ONE_LINE_STATEMENT);
		this.type = type;
	}

	@Override
	public int compareTo(ExpectedType o) {
		return o.type.compareTo(type);
	}
}
