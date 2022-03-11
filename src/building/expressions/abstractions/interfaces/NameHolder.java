package building.expressions.abstractions.interfaces;

import building.expressions.abstractions.Expression;
import building.expressions.main.functions.Definition;
import building.expressions.normal.containers.Name;
import building.expressions.normal.containers.Variable;

/**
 * Everything that has a {@link Name}.
 * 
 * @see Name
 * @see Variable
 * @see Definition
 */
public interface NameHolder {

	/** Returns the {@link Name} object of this {@link Expression}. */
	public Name getName();

	/** Returns the name of this {@link Expression} as a {@link String}. */
	public default String getNameString() {
		Name n = getName();
		return n == null ? "uninitialised" : n.getNameString();
	}
}
