package building.types.specific.operators;

import static building.types.SuperType.POSTFIX_OPERATOR;
import static building.types.specific.ExpressionType.LITERAL;

import building.types.AbstractType;
import building.types.SuperType;

public enum PostfixOpType implements AbstractType {

	// Arithmetic
	INC("++"), DEC("--"), FAC("!");

	/** Necessary constructor and symbol. */

	public final String symbol;

	private PostfixOpType(String s) {
		symbol = s;
	}

	@Override
	public String toString() {
		return symbol;
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == POSTFIX_OPERATOR;
	}

	@Override
	public AbstractType[] expected() {
		return LITERAL.expected();
	}
}