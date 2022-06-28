package building.types.specific.operators;

import static building.types.abstractions.SuperType.*;

import building.expressions.normal.operators.prefix.*;
import building.types.abstractions.*;

/**
 * @see PrefixOperator
 */
public enum PrefixOpType implements SpecificType {
	
	// Arithmetic
	INC("++"),
	DEC("--"),
	NEG("-"),
	
	// Logic
	NOT("not");
	
	final String symbol;
	
	PrefixOpType(String s) {
		symbol = s;
	}
	
	@Override
	public AbstractType[] abstractExpected() {
		return switch (this) {
			case NOT -> new AbstractType[] {VAL_HOLDER_TYPE};
			default -> new AbstractType[] {VAL_HOLDER_TYPE};
		};
	}
	
	@Override
	public String toString() {
		return symbol;
	}
}