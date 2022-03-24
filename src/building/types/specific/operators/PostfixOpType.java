package building.types.specific.operators;

import static building.types.specific.DynamicType.NAME;

import building.expressions.normal.operators.postfix.PostfixOperator;
import building.types.abstractions.AbstractType;
import building.types.abstractions.SpecificType;

/**
 * @see PostfixOperator
 */
public enum PostfixOpType implements SpecificType {

	// Arithmetic
	INC("++"), DEC("--"), FAC("!");

	/** Necessary constructor and symbol. */

	final String symbol;

	PostfixOpType(String s) {
		symbol = s;
	}

	@Override
	public AbstractType[] abstractExpected() {
		return NAME.expected();
	}

	@Override
	public String toString() {
		return symbol;
	}
}