package building.expressions.possible.multicall;

import static building.types.SuperType.MERGED;

import building.expressions.abstractions.PossibleMainExpression;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.statements.IsStatement;
import building.expressions.normal.operators.infix.InOperator;
import building.expressions.normal.operators.infix.LogicalOperator;
import building.expressions.possible.Call;
import building.types.specific.operators.InfixOpType;
import interpreting.exceptions.IllegalCodeFormatException;
import runtime.datatypes.Value;

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
public class MultiCall extends PossibleMainExpression implements ValueHolder {

	private final ValueHolder[] content;
	private final MultiCallable outer;

	/**
	 * Creates a {@link MultiCall}.
	 *
	 * @param outer   shouldn't be null
	 * @param content shouldn't be null
	 */
	public MultiCall(int lineID, MultiCallable outer, ValueHolder[] content) {
		super(lineID, MERGED);
		this.outer = outer;
		this.content = content;
		if (outer == null)
			throw new AssertionError("Outer cannot be null.");
		if (content.length < 2)
			throw new IllegalCodeFormatException(getOriginalLine(), "This Multicall has to contain atleast two elements");
	}

	@Override
	public Value getValue() {
		return outer.executeFor(content);
	}

	@Override
	public boolean execute() {
		getValue();
		return callNextLine();
	}
}
