package types.specific.operators;

import static expressions.normal.operators.infix.InfixOperator.Associativity.LEFT;

import expressions.normal.operators.infix.InfixOperator.Associativity;
import types.AbstractType;
import types.SuperType;

public enum InfixOpType implements AbstractType {

	// Arithmetic
	ADD("+", 6, LEFT), SUB("-", 6, LEFT), MULT("*", 7, LEFT), DIV("/", 7, LEFT), POW("^", 8, LEFT), ROOT("root", 8, LEFT),
	MOD("%", 7, LEFT),

	// Logic
	AND("and", 3, LEFT), NAND("nand", 3, LEFT), OR("or", 2, LEFT), NOR("nor", 2, LEFT), XOR("xor", 2, LEFT), XNOR("xnor", 2, LEFT),

	// Comparison
	EQUALS("==", 4, LEFT), GREATER(">", 5, LEFT), GREATER_EQ(">=", 5, LEFT), LESS("<", 5, LEFT), LESS_EQ("<=", 5, LEFT),
	NOT_EQUALS("!=", 4, LEFT),

	// Misc
	IN("in", 10, LEFT);

	public final int rank;

	public final Associativity associativity;

	public final String symbol;

	private InfixOpType(String s, int r, Associativity a) {
		this.symbol = s;
		this.rank = r;
		this.associativity = a;
	}

	@Override
	public String toString() {
		return symbol;
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.INFIX_OPERATOR;
	}

	@Override
	public AbstractType[] expected() {
		return VAL_HOLDER_TYPES;
	}
}