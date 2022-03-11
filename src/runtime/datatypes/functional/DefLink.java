package runtime.datatypes.functional;

import building.expressions.abstractions.interfaces.Callable;
import building.expressions.abstractions.interfaces.NameHolder;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.functions.Definition;
import building.expressions.normal.containers.Name;
import interpreting.exceptions.IllegalCodeFormatException;
import runtime.datatypes.Value;
import runtime.datatypes.textual.TextValue;
import runtime.defmanager.DefManager;
import runtime.exceptions.UnexpectedTypeError;

/**
 * A link to an existing, reachable function.
 * 
 * <pre>
 * Format: name<count_of_params> 
 * For example: print<1>
 * </pre>
 */
public class DefLink extends DefValue implements Callable, NameHolder {

	/** The name of the targetted def. */
	private final Name target;

	/** The amount of params. */
	private final int paramCnt;

	public DefLink(Name target, int paramCnt) {
		if (target == null)
			throw new AssertionError("Name cannot be null.");
		this.target = target;
		this.paramCnt = paramCnt;
	}

	@Override
	public TextValue asText() {
		return new TextValue(getNameString() + "<" + paramCnt + ">");
	}

	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
		if (v instanceof DefLink def)
			return target.equals(def.target) && paramCnt == def.paramCnt;
		return false;
	}

	@Override
	public Value call(ValueHolder... params) {
		if (params.length != paramCnt)
			new IllegalCodeFormatException(getOriginalLine(),
					"The def " + getName() + " was defined to have " + paramCnt + " params, but was called with " + params.length + ".");
		return raw().call(params);
	}

	/** Returns the {@link Definition} that this {@link DefLink} links to. */
	@Override
	public final Definition raw() {
		return DefManager.get(target.getNameString(), paramCnt, target.getOriginalLine());
	}

	@Override
	public Name getName() {
		return target;
	}

}
