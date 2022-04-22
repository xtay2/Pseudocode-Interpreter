package runtime.exceptions;

import building.expressions.normal.containers.Variable;

/** Gets thrown when null value gets assigned to a {@link Variable} that has no maybe-type. */
@SuppressWarnings("serial")
public class NullNotAllowedException extends AbstractRuntimeException {

	public NullNotAllowedException(int line, String message) {
		super(line, message);
	}

}
