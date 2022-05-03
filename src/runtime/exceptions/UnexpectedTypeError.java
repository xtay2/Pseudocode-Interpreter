package runtime.exceptions;

import building.types.abstractions.AbstractType;
import interpreting.modules.merger.ExpressionMerger;

@SuppressWarnings("serial")
public class UnexpectedTypeError extends Error {

	/**
	 * Gets thrown for unexpected {@link AbstractType}s (mostly in default cases in any)
	 * {@link ExpressionMerger}.
	 *
	 * @param type is the unexpected {@link AbstractType}.
	 * @param target is the {@link Class} of the object that was meant to be build.
	 */
	public UnexpectedTypeError(int orgLine, AbstractType type, Class<?> target) {
		super(type + " is an unexpected type for a " + target.getSimpleName());
	}
}
