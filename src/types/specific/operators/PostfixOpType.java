package types.specific.operators;

import expressions.abstractions.Expression;
import types.AbstractType;
import types.SuperType;

/**
 * @deprecated WIP: Don't use this, its only implemented here.
 */
@Deprecated
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
	public Expression create(String arg, int lineID) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.POSTFIX_OPERATOR;
	}
}