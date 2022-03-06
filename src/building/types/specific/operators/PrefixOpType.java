package building.types.specific.operators;

import building.types.AbstractType;
import building.types.SuperType;

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
		return AbstractType.valHolderTypes();
	}
}