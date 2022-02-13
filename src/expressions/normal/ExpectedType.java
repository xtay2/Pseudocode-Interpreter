package expressions.normal;
import static parsing.program.ExpressionType.NAME;
import static parsing.program.ExpressionType.OPEN_SCOPE;

import expressions.abstractions.Expression;
import expressions.special.DataType;
import parsing.program.ExpressionType;

public class ExpectedType extends Expression implements Comparable<ExpectedType> {

	public final DataType type;

	public ExpectedType(DataType type, int line) {
		super(line, ExpressionType.EXPECTED_TYPE);
		setExpectedExpressions(NAME, OPEN_SCOPE);
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
