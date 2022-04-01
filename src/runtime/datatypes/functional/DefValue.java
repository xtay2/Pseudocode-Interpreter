package runtime.datatypes.functional;

import static building.types.specific.DataType.DEF;

import building.types.specific.DataType;
import runtime.datatypes.Value;

/**
 * Superclass for {@link LambdaValue} and {@link DefLink}.
 */
public abstract class DefValue extends Value {

	@Deprecated
	public DefValue() {
		super(DEF);
		throw new AssertionError("The def-type is currently WIP.");
	}

	/** A {@link DefValue} can get cast to nothing except NaN and its Text rep. */
	@Override
	public final boolean canCastTo(DataType type) {
		return switch (type) {
			case VAR, DEF -> true; // Returns itself
			case TEXT -> true; // Returns Text-Representation
			case NUMBER -> true; // Allways returns NaN
			default -> false;
		};
	}

	@Override
	public final DefValue asDef() {
		return this;
	}

}
