package runtime.datatypes.functional;

import static building.types.specific.data.DataType.DEF;

import building.types.specific.data.DataType;
import runtime.datatypes.Value;

/**
 * Superclass for {@link LambdaValue} and {@link DefLink}.
 */
public abstract class DefValue extends Value {

	public DefValue() {
		super(DEF);
	}

	/** A {@link DefValue} can get cast to nothing except NaN and its Text rep. */
	@Override
	public final boolean canCastTo(DataType type) {
		return switch (type) {
			case VAR, DEF -> true; // Returns itself
			case TEXT -> true; // Returns Text-Representation
			case NUMBER -> true; // Allways returns NaN
			case INT, BOOL, CHAR, OBJECT -> false;
		};
	}

	@Override
	public final DefValue asDef() {
		return this;
	}

}
