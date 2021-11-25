package programreader.expressions.normal;

import java.util.ArrayList;

import programreader.expressions.special.Expression;
import programreader.expressions.special.Operator;
import programreader.expressions.special.Value;
import programreader.expressions.special.ValueHolder;

/**
 * Consist of n Operators and n + 1 ValueHolders.
 */
public final class Operation extends Expression implements ValueHolder {

	private final ArrayList<Expression> operation;

	public Operation(int line, ArrayList<Expression> op) {
		super(line);
		this.operation = op;
		if (operation.size() < 3)
			throw new IllegalArgumentException("An operation has to atleast contain one operator and two values.");
	}

	@Override
	public Value getValue() {
		return null;//recValue(0, 1, 2);
	}

//	private Value recValue(int valA, int operator, int valB) {
//		ValueHolder a = (ValueHolder) operation.get(valA);
//		ValueHolder b = (ValueHolder) operation.get(valB);
//		Operator o = (Operator) operation.get(operator);
//		Operator next = o <
//		if()
//	}
}
