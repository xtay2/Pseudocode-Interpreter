package building.types.abstractions;

/**
 * Super-Interface for all types.
 *
 * @see SpecificType
 * @see UnspecificType
 * @see SuperType
 */
public sealed interface AbstractType permits SpecificType, UnspecificType {
	
	/**
	 * Should tell if this can be allways described by other in any way.
	 *
	 * Returns allways true if this == other.
	 */
	boolean is(AbstractType other);
}
