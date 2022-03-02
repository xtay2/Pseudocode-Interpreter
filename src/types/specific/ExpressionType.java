package types.specific;

import static types.SuperType.*;
import static types.specific.BuilderType.OPEN_SCOPE;

import expressions.normal.BuilderExpression;
import expressions.normal.containers.Name;
import modules.parser.program.ValueBuilder;
import types.AbstractType;
import types.SuperType;

public enum ExpressionType implements AbstractType {

	NAME("Name"),

	LITERAL("Literal");

	public final String expression;

	private ExpressionType(String expression) {
		this.expression = expression;
	}

	@Override
	public BuilderExpression create(String arg, int lineID) {
		if (!switch (this) {
			case LITERAL -> ValueBuilder.isLiteral(arg);
			case NAME -> Name.isName(arg);
		}) {
			return null;
		}
		// Calls the specialised Constructor.
		return new BuilderExpression(lineID, this, arg);
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.EXPRESSION_TYPE;
	}

	@Override
	public String toString() {
		return expression;
	}

	@Override
	public AbstractType[] expected() {
		return switch (this) {
			case NAME -> new AbstractType[] { ASSIGNMENT_TYPE, INFIX_OPERATOR, POSTFIX_OPERATOR, KEYWORD_TYPE, BUILDER_TYPE };
			case LITERAL -> new AbstractType[] { OPEN_SCOPE, INFIX_OPERATOR, KEYWORD_TYPE, BUILDER_TYPE };
		};
	}
}
