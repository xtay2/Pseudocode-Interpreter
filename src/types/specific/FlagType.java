package types.specific;

import types.SpecificType;

/**
 * Modifiers that change the behaviour of certain objects/variables.
 * 
 * Flags are no Keywords.
 */
public enum FlagType implements SpecificType {

	/**
	 * Tells, that the following definition doesn't exist in the code files, but
	 * rather in the Interpreter.
	 */
	NATIVE("native"),

	/**
	 * Tells, that the following variable is completely unchangeable/immutable.
	 */
	CONSTANT("const");

	final String flag;

	FlagType(String flag) {
		this.flag = flag;
	}

	@Override
	public String toString() {
		return flag;
	}
}
