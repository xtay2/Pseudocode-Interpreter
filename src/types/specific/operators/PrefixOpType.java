package types.specific.operators;

import types.AbstractType;
import types.SuperType;

public enum PrefixOpType implements AbstractType {
	// Arithmetic
	INC("++"), DEC("--"),

	// Logic
	NOT("not");

	public final String symbol;

	private PrefixOpType(String s) {
		symbol = s;
	}

	@Override
	public String toString() {
		return symbol;
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.PREFIX_OPERATOR;
	}

	@Override
	public AbstractType[] expected() {
		return VAL_HOLDER_TYPES;
	}
}