package programreader.expressions.special;

import programreader.expressions.normal.operators.arithmetic.AddOperator;
import programreader.expressions.normal.operators.arithmetic.MultOperator;
import programreader.program.ExpressionType;

public abstract class Operator extends Expression {

	public enum Associativity {
		LEFT, RIGHT, NONE;
	}
	
	/** Value that determines the importance of an Operator. */
	public final int rank;
	
	protected Operator(int line, int rank) {
		super(line);
		if (rank < 0 || rank > 10)
			throw new IllegalArgumentException("Rank has to be between 1 and 10.");
		this.rank = rank;
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME);
	}

	public abstract Value perform(ValueHolder a, ValueHolder b);
	
	public abstract Associativity getAssociativity();
	
	public boolean isLeftAssociative() {
		return getAssociativity() == Associativity.LEFT;
	}
	
	public boolean isRightAssociative() {
		return getAssociativity() == Associativity.RIGHT;
	}
	
	enum InfixOperator {
		/** Arithmetic */
		ADD("+"), SUB("-"), MULT("*"), DIV("/"), MOD("%"),

		/** Zuweisungen */
		ADDI("+="), SUBI("-="), MULTI("*="), DIVI("/="), MODI("%="),

		/** Logic */
		EQUALS("=="), NOT_EQUALS("!="), AND("and"), NAND("nand"), OR("or"), NOR("nor"), XOR("xor");

		public final String symbol;

		InfixOperator(String s) {
			symbol = s;
		}
	}

	enum PrefixOperator {
		NOT("!");

		public final String symbol;

		PrefixOperator(String s) {
			symbol = s;
		}
	}

	public static boolean isOperator(String s) {
		for (InfixOperator op : InfixOperator.values()) {
			if (op.symbol.equals(s))
				return true;
		}
		return false;
	}

	public static Operator operatorExpression(String s, int line) {
		/* Arithmetic */
		if (InfixOperator.ADD.symbol.equals(s))
			return new AddOperator(line);
		if (InfixOperator.MULT.symbol.equals(s))
			return new MultOperator(line);
		/* Logic */
		throw new AssertionError(s + " should be known by now.");
	}

}
