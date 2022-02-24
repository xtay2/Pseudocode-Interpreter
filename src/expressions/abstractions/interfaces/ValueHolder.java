package expressions.abstractions.interfaces;

import datatypes.Value;
import expressions.main.statements.IsStatement;
import expressions.normal.brackets.BracketedExpression;
import expressions.normal.containers.ArrayAccess;
import expressions.normal.containers.Name;
import expressions.normal.operators.Operation;
import expressions.possible.Call;
import expressions.possible.assigning.Assignment;
import expressions.possible.multicall.MultiCall;

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
public interface ValueHolder {

	public Value getValue();
}
