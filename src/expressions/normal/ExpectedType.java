package expressions.normal;
import static parsing.program.ExpressionType.NAME;
import static parsing.program.ExpressionType.OPEN_SCOPE;

import expressions.special.DataType;

public class ExpectedType extends Expression implements Comparable<ExpectedType> {

	public final DataType type;

	public ExpectedType(DataType type, int line) {
		super(line);
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
