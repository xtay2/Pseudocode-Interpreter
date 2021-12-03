package expressions.normal;

import expressions.special.Type;
import parser.program.ExpressionType;

/**
 * Variable with enforced type.
 */
public class TypedVar extends Variable {

	public final Type type;

	public TypedVar(String type, int line) {
		this(Type.stringToType(type), line);
	}

	public TypedVar(Type type, int line) {
		super(line);
		setExpectedExpressions(ExpressionType.NAME);
		this.type = type;
	}

	@Override
	public Type getType() {
		return type;
	}

}
