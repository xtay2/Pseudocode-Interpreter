package expressions.normal.operators;

public enum InfixOperator {
	/** Arithmetic */
	ADD("+", 6), SUB("-", 6), MULT("*", 7), DIV("/", 7), POW("^", 8), ROOT("root", 8), MOD("%", 7),

	/** Logic */
	AND("and", 3), NAND("nand", 3), OR("or", 2), NOR("nor", 2), XOR("xor", 2), XNOR("xnor", 2),

	/** Comparison */
	EQUALS("==", 4), GREATER(">", 5), GREATER_EQ(">=", 5), LESS("<", 5), LESS_EQ("<=", 5), NOT_EQUALS("!=", 4),

	/** Misc */
	IN("in", 10);

	public final int rank;

	public final String symbol;

	private InfixOperator(String s, int rank) {
		symbol = s;
		this.rank = rank;
	}

	@Override
	public String toString() {
		return symbol;
	}
}