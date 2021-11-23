package programreader.expressions.normal;

import programreader.expressions.special.Expression;
import programreader.expressions.special.Scope;
import programreader.expressions.special.Value;
import programreader.expressions.special.ValueHolder;
import programreader.interpreter.VarManager;
import programreader.program.ExpressionType;

public class Name extends Expression implements ValueHolder {

	private final String name;
	private Scope scope = null;

	public Name(final String name, int line) {
		super(line);
		setExpectedExpressions(ExpressionType.DECLARATION, ExpressionType.OPEN_BRACKET, ExpressionType.COMMA, ExpressionType.CLOSE_BRACKET,
				ExpressionType.ONE_LINE_STATEMENT, ExpressionType.OPEN_BLOCK, ExpressionType.INFIX_OPERATOR);
		this.name = name;
	}

	@Override
	public Value getValue() {
		return VarManager.get(name).getValue();
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

	@Override
	public String toString() {
		return "Name: \"" + name + "\"";
	}

}
