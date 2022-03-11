package building.types.specific.operators;

import static building.expressions.normal.operators.infix.InfixOperator.Associativity.LEFT;
import static building.types.SuperType.EXPECTED_TYPE;
import static building.types.SuperType.EXPRESSION_TYPE;
import static building.types.specific.BuilderType.*;

import building.expressions.normal.operators.infix.InfixOperator.Associativity;
import building.types.AbstractType;
import building.types.SuperType;

public enum InfixOpType implements AbstractType {

	// Arithmetic
	ADD("+", 6, LEFT), SUB("-", 6, LEFT), MULT("*", 7, LEFT), DIV("/", 7, LEFT), POW("^", 8, LEFT), ROOT("root", 8, LEFT),
	MOD("%", 7, LEFT),

	// Logic
	AND("and", 3, LEFT), NAND("nand", 3, LEFT), OR("or", 2, LEFT), NOR("nor", 2, LEFT), XOR("xor", 2, LEFT), XNOR("xnor", 2, LEFT),

	// Comparison
	EQUALS("eq", 4, LEFT), GREATER(">", 5, LEFT), GREATER_EQ(">=", 5, LEFT), LESS("<", 5, LEFT), LESS_EQ("<=", 5, LEFT),
	NOT_EQUALS("neq", 4, LEFT),

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
		if (this == GREATER)
			return new AbstractType[] { ARRAY_START, OPEN_BRACKET, EXPECTED_TYPE, EXPRESSION_TYPE, COMMA, CLOSE_BRACKET, ARRAY_END };
		else
			return AbstractType.valHolderTypes();
	}
}