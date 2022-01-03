package expressions.normal.operators;

public enum InfixOperator {
	/** Arithmetic */
	ADD("+", 6), SUB("-", 6), MULT("*", 7), DIV("/", 7), MOD("%", 7), POW("^", 8),

	/** Zuweisungen */
	ADDI("+=", 1), SUBI("-=", 1), MULTI("*=", 1), DIVI("/=", 1), MODI("%=", 1),

	/** Comparison */
	EQUALS("==", 4), NOT_EQUALS("!=", 4), LESS("<", 5), LESS_EQ("<=", 5), GREATER(">", 5), GREATER_EQ(">=", 5),

	/** Logic */
	AND("and", 3), NAND("nand", 3), OR("or", 2), NOR("nor", 2), XOR("xor", 2);

	public final String symbol;

	public final int rank;

	private InfixOperator(String s, int rank) {
		symbol = s;
		this.rank = rank;
	}

	@Override
	public String toString() {
		return symbol;
	}
}