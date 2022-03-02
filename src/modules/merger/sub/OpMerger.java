package modules.merger.sub;

import static types.SuperType.INFIX_OPERATOR;

import java.util.ArrayList;
import java.util.List;

import expressions.abstractions.interfaces.Operatable;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.operators.Operation;
import expressions.normal.operators.infix.ArithmeticOperator;
import expressions.normal.operators.infix.ComparativeOperator;
import expressions.normal.operators.infix.InOperator;
import expressions.normal.operators.infix.InfixOperator;
import expressions.normal.operators.infix.LogicalOperator;
import expressions.normal.operators.postfix.PostfixOperator;
import expressions.normal.operators.prefix.PrefixOperator;
import types.specific.operators.InfixOpType;
import types.specific.operators.PostfixOpType;
import types.specific.operators.PrefixOpType;

/**
 * Every merged {@link Operation}, {@link PrefixOperator}, {@link InfixOperator} or
 * {@link PostfixOperator}.
 */
public abstract class OpMerger extends ValueMerger {

	/** [PRE_FIX] [VALUE_CHANGER] */
	public static PrefixOperator buildPrefix() {
		PrefixOpType t = (PrefixOpType) line.remove(0).type;
		ValueChanger vc = (ValueChanger) buildVal();
		return new PrefixOperator(lineID, t, vc);
	}

	/** [POST_FIX] */
	public static PostfixOperator buildPostfix(ValueChanger vc) {
		PostfixOpType t = (PostfixOpType) line.remove(0).type;
		return new PostfixOperator(lineID, t, vc);
	}

	// --------------------------------INFIX--------------------------------//

	/** [ValueHolder] ([Operator] [ValueHolder])... */
	public static Operation buildOperation(ValueHolder fst) {
		List<Operatable> parts = new ArrayList<>();
		parts.add(fst);
		while (!line.isEmpty() && line.get(0).is(INFIX_OPERATOR)) {
			parts.add(buildInfix());
			parts.add(buildVal());
		}
		return new Operation(lineID, parts);
	}

	/** [INFIX_OPERATOR] */
	private static InfixOperator buildInfix() {
		InfixOpType type = (InfixOpType) line.remove(0).type;
		return switch (type) {
			// Arithmetic
			case ADD, SUB, MULT, DIV, MOD, POW, ROOT -> new ArithmeticOperator(lineID, type);
			// Comparison
			case EQUALS, NOT_EQUALS, GREATER, GREATER_EQ, LESS, LESS_EQ -> new ComparativeOperator(lineID, type);
			// Logical
			case AND, NAND, OR, NOR, XOR, XNOR -> new LogicalOperator(lineID, type);
			// Misc
			case IN -> new InOperator(lineID, type);
		};
	}
}
