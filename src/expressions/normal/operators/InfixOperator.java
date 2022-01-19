package expressions.normal.operators;

public enum InfixOperator {
	/** Arithmetic */
	ADD("+", 6), /** Logic */
	AND("and", 3), DIV("/", 7), /** Comparison */
	EQUALS("==", 4), GREATER(">", 5), GREATER_EQ(">=", 5),

	LESS("<", 5), LESS_EQ("<=", 5), MOD("%", 7), MULT("*", 7), NAND("nand", 3), NOR("nor", 2),

	NOT_EQUALS("!=", 4), OR("or", 2), POW("^", 8), SUB("-", 6), XNOR("xnor", 2), XOR("xor", 2);

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