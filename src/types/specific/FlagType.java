package types.specific;

import static types.SuperType.*;
import static types.specific.KeywordType.FUNC;

import expressions.abstractions.Expression;
import expressions.normal.BuilderExpression;
import types.AbstractType;
import types.SuperType;

/**
 * Modifiers that change the behaviour of certain objects/variables.
 * 
 * Flags are no Keywords.
 */
public enum FlagType implements AbstractType {

	/**
	 * Tells, that the following definition doesn't exist in the code files, but rather in the
	 * Interpreter.
	 */
	NATIVE("native", FUNC),

	/**
	 * Tells, that the following variable is completely unchangeable/immutable.
	 */
	CONSTANT("const", DATA_TYPE);

	final String flag;

	public final AbstractType[] expected;

	private FlagType(String flag, AbstractType... expected) {
		this.flag = flag;
		this.expected = expected;
	}

	@Override
	public String toString() {
		return flag;
	}

	@Override
	public Expression create(String arg, int lineID) {
		if (!flag.equals(arg.strip()))
			return null;
		return new BuilderExpression(this);
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.FLAG_TYPE;
	}

	/** Checks, if the passed {@link String} is a {@link FlagType}. */
	public static boolean isFlag(String arg) {
		for (FlagType t : FlagType.values()) {
			if (t.flag.equals(arg))
				return true;
		}
		return false;
	}
	
}
