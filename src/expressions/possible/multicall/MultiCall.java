package expressions.possible.multicall;

import static types.SuperType.INFIX_OPERATOR;
import static types.specific.ExpressionType.NAME;

import java.util.Arrays;

import datatypes.Value;
import exceptions.parsing.IllegalCodeFormatException;
import expressions.abstractions.Expression;
import expressions.abstractions.PossibleMainExpression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.statements.IsStatement;
import expressions.normal.operators.infix.InOperator;
import expressions.normal.operators.infix.LogicalOperator;
import expressions.possible.Call;
import types.SuperType;
import types.specific.operators.InfixOpType;

/**
 * Performs the outer Expression for each {@link MultiCallable} in it.
 * 
 * <pre>
 * Should support:
 * 
 *  - Any {@link Crement}-Operation:
 * 	|a, b|++ -> a++, b++
 * 
 *  - Any {@link Call} that takes just one parameter:
 * 	print(|a, b|) -> print(a), print(b)
 * 
 *  - Any {@link LogicalOperator} that is an {@link InfixOpType}, on one of both side:
 * 	a and |b, c| -> (a and b) or (a and c)
 * 	|a, b| or c -> (a or c) or (b or c)
 *  
 *  - The {@link InOperator} on one of both sides.
 * 	|a, b| in c -> (a in c) or (b in c)
 * 	a in |b, c| -> (a in b) or (a in c)
 *  
 *  - The {@link IsStatement}
 * 	|a, b| is type -> (a is type) or (b is type)
 * 
 * Should explicitly not support:
 * 
 *  - An {@link MultiCall} thats somehow connected with anotherone, like:
 *	|a, |b, c||
 *	|a, b| and |c, d|
 *	add(|a, b, c|, |d, e|)
 * </pre>
 */
public class MultiCall extends PossibleMainExpression implements MergedExpression, ValueHolder {

	private ValueHolder[] content;
	private final MultiCallable outer;

	public MultiCall(MultiCallable outer, int line) {
		super(line, SuperType.MERGED, INFIX_OPERATOR, NAME);
		this.outer = outer;
		if (outer == null)
			throw new AssertionError("Outer cannot be null.");
	}

	@Override
	public void merge(Expression... e) {
		if (e.length < 2)
			throw new IllegalCodeFormatException("A MultiCall has to contain atleast two elements.");
		content = Arrays.copyOf(e, e.length, ValueHolder[].class);
	}

	@Override
	public Value getValue() {
		return outer.executeFor(content);
	}

	@Override
	public boolean execute(ValueHolder... params) {
		getValue();
		return callNextLine();
	}
}
