package building.expressions.abstractions.interfaces;

import building.expressions.abstractions.Expression;
import building.expressions.normal.containers.Name;

public interface NameHolder {

	/** Returns the {@link Name} object of this {@link Expression}. */
	public Name getName();

	/** Returns the name of this {@link Expression} as a {@link String}. */
	public default String getNameString() {
		Name n = getName();
		return n == null ? "uninitialised" : n.getNameString();
	}
}
