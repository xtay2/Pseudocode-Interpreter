package building.expressions.normal.operators.infix;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.normal.operators.*;
import building.expressions.possible.multicall.*;
import building.types.specific.operators.*;
import runtime.datatypes.*;

/**
 * Used in an {@link Operation}.
 * 
 * @see ArithmeticOperator
 * @see LogicalOperator
 * @see ComparativeOperator
 * @see InOperator
 * 
 * @see InfixOpType
 */
public abstract class InfixOperator extends Expression implements Operatable, MultiCallableOperation {
	
	public enum Associativity {
		LEFT, RIGHT;
	}
	
	/** Corresponding symbol to this operator; */
	public final InfixOpType op;
	
	protected InfixOperator(int line, InfixOpType op) {
		super(line, op);
		this.op = op;
		if (op.rank < 0)
			throw new AssertionError("Rank cannot be negative.");
	}
	
	public final Associativity getAssociativity() { return op.associativity; }
	
	public boolean isLeftAssociative() { return getAssociativity() == Associativity.LEFT; }
	
	public boolean isRightAssociative() { return getAssociativity() == Associativity.RIGHT; }
	
	public abstract Value perform(ValueHolder a, ValueHolder b);
	
	@Override
	public Value executeFor(ValueHolder operand, ValueHolder[] content) {
		for (int i = 0; i < content.length; i++)
			if (((BoolValue) perform(operand, content[i])).raw())
				return BoolValue.TRUE;
		return BoolValue.FALSE;
	}
	
	@Override
	public Value executeFor(ValueHolder[] content, ValueHolder operand) {
		for (int i = 0; i < content.length; i++)
			if (((BoolValue) perform(content[i], operand)).raw())
				return BoolValue.TRUE;
		return BoolValue.FALSE;
	}
	
}