package building.expressions.abstractions.interfaces;

import building.expressions.main.statements.IsStatement;
import building.expressions.normal.brackets.BracketedExpression;
import building.expressions.normal.containers.ArrayAccess;
import building.expressions.normal.containers.Name;
import building.expressions.normal.operators.Operation;
import building.expressions.possible.Call;
import building.expressions.possible.allocating.Assignment;
import building.expressions.possible.multicall.MultiCall;
import runtime.datatypes.Value;

/**
 * An interface for everything that can return a {@link Value} in code.
 * 
 * <pre>
 * Raw ValueHolders
 * {@link Name} 
 * {@link Call} 
 * {@link Value} 
 * {@link ArrayAccess} 
 * 
 * Boxed ValueHolders
 * {@link IsStatement}
 * {@link Operation}
 * {@link BracketedExpression}
 * {@link Assignment}
 * {@link MultiCall}
 * </pre>
 */
public interface ValueHolder extends Operatable {

	public Value getValue();
}
