package building.expressions.possible.multicall;

import static building.types.abstractions.SpecificType.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.main.statements.*;
import building.expressions.normal.operators.infix.*;
import building.expressions.normal.operators.postfix.*;
import building.expressions.normal.operators.prefix.*;
import building.expressions.possible.*;
import building.types.specific.operators.*;
import errorhandeling.*;
import interpreting.modules.merger.*;
import runtime.datatypes.*;

/**
 * Performs the outer Expression for each {@link MultiCallable} in it.
 *
 * <pre>
 * Should support:
 *
 *  - Any changing {@link PrefixOperator} or {@link PostfixOperator}:
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
 *  - An {@link MultiCall} thats somehow connected with another one, like:
 *	|a, |b, c||
 *	|a, b| and |c, d|
 *	add(|a, b, c|, |d, e|)
 * </pre>
 */
public class MultiCall extends Expression implements ValueHolder {
	
	public final ValueHolder[] content;
	
	/**
	 * Creates a {@link MultiCall}.
	 *
	 * @param outer shouldn't be null
	 * @param content shouldn't be null
	 */
	public MultiCall(int lineID, ValueHolder[] content) {
		super(lineID, MERGED);
		this.content = content;
		if (content.length < 2)
			throw new PseudocodeException("MultiCallParams", "This Multicall has to contain atleast two elements", getBlueprintPath());
	}
	
	/**
	 * Because multicalls are allways wrapped inside a bigger {@link ValueHolder}, they only have the
	 * status of a {@link ValueHolder}, but not the functionality. This is necessarry, so that they can
	 * get detected by the {@link SuperMerger}.
	 */
	@Override
	public Value getValue() {
		throw new UnsupportedOperationException("A multicall holds no direct value but should access the value-holders around it.");
	}
}
