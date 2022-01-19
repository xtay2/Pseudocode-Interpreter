package expressions.normal;

import static parsing.program.ExpressionType.ARRAY_END;
import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.CLOSE_BRACKET;
import static parsing.program.ExpressionType.COMMA;
import static parsing.program.ExpressionType.CREMENT;
import static parsing.program.ExpressionType.DECLARATION;
import static parsing.program.ExpressionType.DEFINITE_LINEBREAK;
import static parsing.program.ExpressionType.INFIX_OPERATOR;
import static parsing.program.ExpressionType.LOOP_CONNECTOR;
import static parsing.program.ExpressionType.OPEN_BLOCK;
import static parsing.program.ExpressionType.OPEN_BRACKET;
import static parsing.program.ExpressionType.OPERATION_ASSIGNMENT;

import datatypes.Value;
import expressions.special.Expression;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.VarManager;

public class Name extends Expression implements ValueHolder {

	/**
	 * Arg is valid name if alphanumerical with underscores
	 */
	public static boolean isName(String arg) {
		return arg.matches("([a-z]|[A-Z]|[0-9]|_)+");
	}
	private final String name;

	private Scope scope = null;

	public Name(final String name, int line) {
		super(line);
		setExpectedExpressions(DECLARATION, OPERATION_ASSIGNMENT, OPEN_BRACKET, COMMA, CLOSE_BRACKET, OPEN_BLOCK, INFIX_OPERATOR,
				LOOP_CONNECTOR, ARRAY_START, ARRAY_END, DEFINITE_LINEBREAK, CREMENT);
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public Scope getScope() {
		return scope;
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

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "\"" + name + "\"";
	}
}
