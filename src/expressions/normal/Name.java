package expressions.normal;

import datatypes.Value;
import expressions.special.Expression;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.VarManager;
import parsing.program.ExpressionType;

public class Name extends Expression implements ValueHolder {

	private final String name;
	private Scope scope = null;

	public Name(final String name, int line) {
		super(line);
		setExpectedExpressions(ExpressionType.DECLARATION, ExpressionType.OPEN_BRACKET, ExpressionType.COMMA, ExpressionType.CLOSE_BRACKET,
				ExpressionType.OPEN_BLOCK, ExpressionType.INFIX_OPERATOR, ExpressionType.LOOP_CONNECTOR, ExpressionType.ARRAY_START,
				ExpressionType.ARRAY_END, ExpressionType.DEFINITE_LINEBREAK);
		this.name = name;
	}

	@Override
	public Value getValue() {
		return VarManager.get(name, getOriginalLine()).getValue();
	}

	public void initScope(Scope scope) {
		this.scope = scope;
		if (scope == null)
			throw new AssertionError("Scope cannot be null!");
	}

	public String getName() {
		return name;
	}

	public Scope getScope() {
		return scope;
	}

	/**
	 * Arg is valid name if alphanumerical with underscores
	 */
	public static boolean isName(String arg) {
		return arg.matches("([a-z]|[A-Z]|[0-9]|_)+");
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "\"" + name + "\"";
	}
}
