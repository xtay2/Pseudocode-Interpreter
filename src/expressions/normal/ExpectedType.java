package expressions.normal;
import static parsing.program.ExpressionType.NAME;
import static parsing.program.ExpressionType.OPEN_BLOCK;

import expressions.special.DataType;
import expressions.special.Expression;
import helper.Output;

public class ExpectedType extends Expression implements Comparable<ExpectedType> {

	public final DataType type;

	public ExpectedType(DataType type, int line) {
		super(line);
		setExpectedExpressions(NAME, OPEN_BLOCK);
		this.type = type;
	}

	public ExpectedType(String type, int line) {
		this(DataType.stringToType(type), line);
	}

	@Override
	public int compareTo(ExpectedType o) {
		return o.type.compareTo(type);
	}
	
	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : type.toString();
	}
}
