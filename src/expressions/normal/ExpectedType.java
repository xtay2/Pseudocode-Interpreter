package expressions.normal;

import expressions.special.Expression;
import expressions.special.Type;
import helper.Output;
import parsing.program.ExpressionType;

public class ExpectedType extends Expression implements Comparable<ExpectedType> {

	public final Type type;

	public ExpectedType(String type, int line) {
		this(Type.stringToType(type), line);
	}

	public ExpectedType(Type type, int line) {
		super(line);
		setExpectedExpressions(ExpressionType.NAME, ExpressionType.OPEN_BLOCK);
		this.type = type;
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
