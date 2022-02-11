package expressions.normal.operators;

public abstract class OperatorTypes {

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

	/**
	 * @deprecated WIP: Don't use this, its only implemented here.
	 */
	public enum PrefixOperator {
		/** Logic */
		NOT("not");

		/** Necessary constructor/rank and symbol. */

		public final int rank = 11;

		public final String symbol;

		private PrefixOperator(String s) {
			symbol = s;
		}

		@Override
		public String toString() {
			return symbol;
		}
	}
	
	/**
	 * @deprecated WIP: Don't use this, its only implemented here.
	 */
	@Deprecated
	public enum PostfixOperator {

		/** Arithmetic */
		FAC("!");

		/** Necessary constructor/rank and symbol. */

		public final int rank = 11;

		public final String symbol;

		private PostfixOperator(String s) {
			symbol = s;
		}

		@Override
		public String toString() {
			return symbol;
		}
	}
}