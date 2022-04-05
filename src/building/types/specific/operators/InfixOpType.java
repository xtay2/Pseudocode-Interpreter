package building.types.specific.operators;

import static building.expressions.normal.operators.infix.InfixOperator.Associativity.LEFT;
import static building.types.abstractions.SuperType.VAL_HOLDER_TYPE;

import building.expressions.normal.operators.Operation;
import building.expressions.normal.operators.infix.InfixOperator.Associativity;
import building.types.abstractions.AbstractType;
import building.types.abstractions.SpecificType;

/**
 * All ranks and associativities for infix operators.
 * 
 * @see Operation
 */
public enum InfixOpType implements SpecificType {

	// Arithmetic
	ADD("+", 6, LEFT), SUB("-", 6, LEFT), MULT("*", 7, LEFT), DIV("/", 7, LEFT), POW("^", 8, LEFT), ROOT("root", 8, LEFT),
	MOD("%", 7, LEFT),

	// Logic
	AND("and", 3, LEFT), NAND("nand", 3, LEFT), OR("or", 2, LEFT), NOR("nor", 2, LEFT), XOR("xor", 2, LEFT), XNOR("xnor", 2, LEFT),

	// Comparison
	EQUALS("==", 4, LEFT), GREATER(">", 5, LEFT), GREATER_EQ("≥", 5, LEFT), LESS("<", 5, LEFT), LESS_EQ("≤", 5, LEFT),
	NOT_EQUALS("≠", 4, LEFT),

	// Misc
	IN("in", 10, LEFT);

	final String symbol;

	public final int rank;

	public final Associativity associativity;

	InfixOpType(String symbol, int r, Associativity a) {
		this.symbol = symbol;
		this.rank = r;
		this.associativity = a;
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