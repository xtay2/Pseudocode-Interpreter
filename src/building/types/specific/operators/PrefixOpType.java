package building.types.specific.operators;

import static building.types.abstractions.SuperType.VAL_HOLDER_TYPE;

import building.expressions.normal.operators.prefix.PrefixOperator;
import building.types.abstractions.AbstractType;
import building.types.abstractions.SpecificType;

/**
 * @see PrefixOperator
 */
public enum PrefixOpType implements SpecificType {

	// Arithmetic
	INC("++"), DEC("--"), NEG("-"),

	// Logic
	NOT("not");

	final String symbol;

	PrefixOpType(String s) {
		symbol = s;
	}

	@Override
	public AbstractType[] abstractExpected() {
		return new AbstractType[] { VAL_HOLDER_TYPE };
	}

	@Override
	public String toString() {
		return symbol;
	}
}