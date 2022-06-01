package building.expressions.abstractions.interfaces;

import building.expressions.abstractions.Scope;
import building.expressions.main.functions.MainFunction;
import building.expressions.normal.containers.Variable;
import interpreting.modules.merger.ExpressionMerger;

/**
 * The Super-Interface for everything that can get registered inside a {@link Scope}.
 *
 * Should get registered by {@link ExpressionMerger#initScopes} or in their constructor at runtime.
 *
 * @see Variable
 * @see MainFunction
 */
public interface Registerable extends NameHolder {

	// Shouldn't override the JavaDoc
	public Scope getScope();
}
