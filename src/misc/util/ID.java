package misc.util;

import java.util.*;

import building.expressions.abstractions.interfaces.*;

/**
 * Identifies a {@link Identifieable}.
 *
 * @param <T> is the type that can get identified by this.
 * @see Identifieable
 */
public class ID<T extends Identifieable<?>> {
	
	private final Object[] identifiers;
	
	/** Pass only the objects that are necessary for unique identification. */
	public ID(Object... objects) {
		this.identifiers = objects;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof ID<?> id)
			return Arrays.equals(identifiers, id.identifiers);
		if (obj instanceof Identifieable<?> val)
			return equals(val.generateID());
		return false;
	}
	
	@Override
	public int hashCode() {
		return Arrays.hashCode(identifiers);
	}
}