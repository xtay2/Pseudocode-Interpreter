package building.types.specific;

import static building.types.SuperType.EXPECTED_TYPE;
import static building.types.specific.ExpressionType.NAME;
import static building.types.specific.KeywordType.FUNC;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;

import building.expressions.normal.containers.Variable;
import building.types.AbstractType;
import building.types.SuperType;
import interpreting.modules.formatter.Formatter;

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
	NATIVE("native", 2),

	/**
	 * Tells, that the value of the following variable can only get defined one.
	 * 
	 * Assures that a function can only get called once.
	 */
	FINAL("final", 1),

	/**
	 * Tells, that a following {@link Variable} is completely unchangeable/immutable.
	 * 
	 */
	CONSTANT("const", 1);

	public final String flag;

	/**
	 * Determines the order of flags. A low number tells, that the flag is rather at the beginning of
	 * the line than in the middle.
	 */
	final int rank;

	private FlagType(String flag, int rank) {
		this.flag = flag;
		this.rank = rank;
	}

	@Override
	public String toString() {
		return flag;
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.FLAG_TYPE;
	}

	/** Checks, if the passed {@link String} is a {@link FlagType}. */
	public static boolean isFlag(String arg) {
		return flagFromString(arg) != null;
	}

	/** Returns a FlagType, if the passed String matches any. */
	private static FlagType flagFromString(String arg) {
		for (FlagType t : values()) {
			if (t.flag.equals(arg))
				return t;
		}
		return null;
	}

	/**
	 * Orders a list of flags, removes the duplicates and unnecessary ones.
	 * 
	 * Gets called in {@link Formatter#orderFlags}
	 */
	public static List<String> orderFlags(List<String> flags) {
		// Remove duplicates
		flags = new ArrayList<String>(new HashSet<String>(flags));
		// Remove unnecessary.
		if (flags.contains(FINAL.flag) && flags.contains(CONSTANT.flag))
			flags.remove(flags.indexOf(FINAL.flag));

		// Order
		flags.sort(new Comparator<String>() {
			@Override
			public int compare(String o1, String o2) {
				FlagType f1 = flagFromString(o1);
				FlagType f2 = flagFromString(o2);
				return Integer.compare(f1.rank, f2.rank);
			}
		});
		return flags;
	}

	@Override
	public AbstractType[] expected() {
		return switch (this) {
			case NATIVE -> new AbstractType[] { FUNC };
			case FINAL -> new AbstractType[] { EXPECTED_TYPE, NAME, NATIVE, FUNC };
			case CONSTANT -> new AbstractType[] { EXPECTED_TYPE, NAME };
		};
	}
}
