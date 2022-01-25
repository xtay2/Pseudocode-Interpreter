package expressions.normal;

import static parsing.program.ExpressionType.ARRAY_END;
import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.ASSIGNMENT;
import static parsing.program.ExpressionType.CLOSE_BRACKET;
import static parsing.program.ExpressionType.COMMA;
import static parsing.program.ExpressionType.CREMENT;
import static parsing.program.ExpressionType.INFIX_OPERATOR;
import static parsing.program.ExpressionType.LOOP_CONNECTOR;
import static parsing.program.ExpressionType.OPEN_BRACKET;
import static parsing.program.ExpressionType.OPEN_SCOPE;
import static parsing.program.ExpressionType.OPERATION_ASSIGNMENT;

import datatypes.Value;
import expressions.special.Scope;
import expressions.special.ValueChanger;
import helper.Output;
import interpreter.VarManager;

public class Name extends Expression implements ValueChanger {


	private final String name;

	private Scope scope = null;

	public Name(String name, int line) {
		super(line);
		setExpectedExpressions(ASSIGNMENT, OPERATION_ASSIGNMENT, OPEN_BRACKET, COMMA, CLOSE_BRACKET, OPEN_SCOPE, INFIX_OPERATOR,
				LOOP_CONNECTOR, ARRAY_START, ARRAY_END, CREMENT);
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

	
	@Override
	public void setValue(Value val) {
		VarManager.get(name, getOriginalLine()).setValue(val);
	}
	
	
	public void initScope(Scope scope) {
		this.scope = scope;
		if (scope == null)
			throw new AssertionError("Scope cannot be null!");
	}
	
	/**
	 * Arg is valid name if alphanumerical with underscores.
	 */
	public static boolean isName(String arg) {
		return arg.matches("\\w*([a-z]|[A-Z])+\\w*");
	}
	
	@Override
	public String toString() {
		return Output.DEBUG ? getClass().getSimpleName() : name;
	}
}
