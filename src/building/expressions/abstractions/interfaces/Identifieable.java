package building.expressions.abstractions.interfaces;

import misc.util.*;

/**
 * Anything that implements this interface is uniquely identifieable by the {@link ID} returned by
 * {@link Identifieable#generateID}.
 *
 * @see ID
 *
 * @param <THIS> should be this class.
 */
public interface Identifieable<THIS extends Identifieable<?>> extends AbstractExpression {
	
	/** Generates a unique {@link ID} for this Object. */
	ID<THIS> generateID();
	
}
