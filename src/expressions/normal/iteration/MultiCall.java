package expressions.normal.iteration;

import datatypes.Value;
import exceptions.parsing.IllegalCodeFormatException;
import expressions.main.statements.IsStatement;
import expressions.normal.Expression;
import expressions.normal.operators.InOperator;
import expressions.normal.operators.OperatorTypes.InfixOperator;
import expressions.normal.operators.logic.LogicalOperator;
import expressions.possible.Call;
import expressions.possible.Crement;
import expressions.possible.PossibleMainExpression;
import expressions.special.MergedExpression;
import expressions.special.ValueHolder;

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
 *  - Any {@link LogicalOperator} that is an {@link InfixOperator}, on one of both side:
 * 	a and |b, c| -> (a and b) or (a and c)
 * 	|a, b| or c -> (a or c) or (b or c)
 *  
 *  - The {@link InOperator} on one of both sides.
 * 	|a, b| in c -> (a in c) or (b in c)
 * 	a in |b, c| -> (a in b) or (a in c)
 *  
 *  - The {@link IsStatement}
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
		super(line);
		this.outer = outer;
	}

	@Override
	public void merge(Expression... e) {
		if (e.length < 2)
			throw new IllegalCodeFormatException("A Iteration has to contain atleast two elements.");
		content = (ValueHolder[]) e;
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
