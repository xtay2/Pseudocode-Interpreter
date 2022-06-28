package building.types.abstractions;

import java.util.*;

/**
 * @see SuperType
 */
public non-sealed interface UnspecificType extends AbstractType {
	
	/** Returns true, if this is == other or other {@link #has()} this. */
	@Override
	default boolean is(AbstractType other) {
		return other instanceof UnspecificType ut && (this == other || ut.has(this));
	}
	
	/** Returns true, if other lies anywhere below this "node". (BFS) */
	default boolean has(UnspecificType other) {
		return Arrays.stream(directSubValues()).anyMatch(sub -> sub instanceof UnspecificType ut && (sub == other || ut.has(other)));
	}
	
	/** Returns the direct values below this "node". */
	AbstractType[] directSubValues();
	
	/** Returns all leafs below this this "node". */
	public default SpecificType[] subValues() {
		List<SpecificType> subValues = new ArrayList<>();
		for (AbstractType t : directSubValues()) {
			if (t instanceof SpecificType st)
				subValues.add(st);
			else if (t instanceof UnspecificType ut)
				subValues.addAll(Arrays.asList(ut.subValues()));
		}
		return subValues.toArray(new SpecificType[subValues.size()]);
	}
}
