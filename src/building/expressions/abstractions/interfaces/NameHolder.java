package building.expressions.abstractions.interfaces;

import building.expressions.abstractions.*;
import building.expressions.main.functions.*;
import building.expressions.normal.containers.*;
import building.expressions.normal.containers.name.*;

/**
 * Everything that has a {@link Name}.
 *
 * @see Name
 * @see Variable
 * @see Definition
 */
public interface NameHolder extends AbstractExpression {
	
	/** Returns the {@link Name} object of this {@link Expression}. */
	public Name getName();
	
	/** Returns the name of this {@link Expression} as a {@link String}. */
	public default String getNameString() {
		Name n = getName();
		return n == null ? "uninitialised" : n.getNameString();
	}
}
