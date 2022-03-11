package building.types.specific;

import static building.types.SuperType.*;
import static building.types.specific.BuilderType.OPEN_BLOCK;

import building.expressions.normal.BuilderExpression;
import building.expressions.normal.containers.Name;
import building.types.AbstractType;
import building.types.SuperType;
import interpreting.program.ValueBuilder;

public enum ExpressionType implements AbstractType {

	LITERAL("Literal"),

	NAME("Name");

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
			case LITERAL -> new AbstractType[] { OPEN_BLOCK, INFIX_OPERATOR, KEYWORD_TYPE, BUILDER_TYPE };
		};
	}
}
