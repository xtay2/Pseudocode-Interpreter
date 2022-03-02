package types.specific.operators;

import static types.SuperType.POSTFIX_OPERATOR;
import static types.specific.ExpressionType.LITERAL;

import types.AbstractType;
import types.SuperType;

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