package exceptions.runtime;

import expressions.abstractions.Scope;

/** Gets thrown by {@link Scope#get()}. */
@SuppressWarnings("serial")
public class VarNotFoundException extends AbstractRuntimeException {

	public VarNotFoundException(int line, String message) {
		super(line, message);
	}

}
