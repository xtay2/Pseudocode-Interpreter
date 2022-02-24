package types.specific.operators;

import expressions.abstractions.Expression;
import types.AbstractType;
import types.SuperType;

/**
 * @deprecated WIP: Don't use this, its only implemented here.
 */
public enum PrefixOpType implements AbstractType {
	// Arithmetic
	INC("++"), DEC("--"),

	// Logic
	NOT("not");

	/** Necessary constructor and symbol. */

	public final String symbol;

	private PrefixOpType(String s) {
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
		return superType == SuperType.PREFIX_OPERATOR;
	}
}