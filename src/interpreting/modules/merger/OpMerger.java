package interpreting.modules.merger;

import static building.types.abstractions.SuperType.*;

import java.util.*;

import building.expressions.abstractions.interfaces.*;
import building.expressions.normal.operators.*;
import building.expressions.normal.operators.infix.*;
import building.expressions.normal.operators.postfix.*;
import building.expressions.normal.operators.prefix.*;
import building.types.specific.operators.*;

/**
 * Every merged {@link Operation}, {@link PrefixOperator}, {@link InfixOperator} or
 * {@link PostfixOperator}.
 */
public abstract class OpMerger extends ValueMerger {
	
	/** [PRE_FIX] [VALUE_CHANGER] */
	public static PrefixOperator buildPrefix() {
		PrefixOpType t = (PrefixOpType) line.remove(0).type;
		ValueHolder vc = buildVal(true);
		return new PrefixOperator(lineID, t, vc);
	}
	
	/** [POST_FIX] */
	public static PostfixOperator buildPostfix(ValueHolder vc) {
		PostfixOpType t = (PostfixOpType) line.remove(0).type;
		return new PostfixOperator(lineID, t, vc);
	}
	
	// --------------------------------INFIX--------------------------------//
	
	/** [ValueHolder] ([Operator] [ValueHolder])... */
	public static Operation buildOperation(ValueHolder fst) {
		List<Operatable> parts = new ArrayList<>();
		parts.add(fst);
		while (!line.isEmpty() && line.get(0).is(INFIX_OP_TYPE)) {
			parts.add(buildInfix());
			parts.add(buildVal(true));
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
