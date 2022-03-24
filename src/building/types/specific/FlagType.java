package building.types.specific;

import static building.types.abstractions.SpecificType.fromString;
import static building.types.abstractions.SuperType.DATA_TYPE;
import static building.types.specific.BuilderType.OPEN_BLOCK;
import static building.types.specific.DynamicType.NAME;
import static building.types.specific.KeywordType.FUNC;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;

import building.expressions.normal.containers.Variable;
import building.types.abstractions.AbstractType;
import building.types.abstractions.SpecificType;

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
    NATIVE("native", 2),

    /**
     * Tells, that the value of the following variable can only get defined one.
     * 
     * Assures that a function can only get called once.
     */
    FINAL("final", 1),

    /**
     * Tells, that a following {@link Variable} is completely
     * unchangeable/immutable.
     * 
     */
    CONSTANT("const", 1),

    /**
     * Has no implementation yet.
     */
    @Deprecated()
    PUBLIC("public", 0);

    final String symbol;

    /**
     * Determines the order of flags. A low number tells, that the flag is rather at
     * the beginning of the line than in the middle.
     */
    final int rank;

    private FlagType(String flag, int rank) {
	symbol = flag;
	this.rank = rank;
    }

    @Override
    public AbstractType[] abstractExpected() {
	return switch (this) {
	    case NATIVE -> new AbstractType[] { FUNC };
	    case FINAL -> new AbstractType[] { DATA_TYPE, NATIVE, OPEN_BLOCK, FUNC, NAME };
	    case CONSTANT -> new AbstractType[] { DATA_TYPE, OPEN_BLOCK, NAME };
	    case PUBLIC -> new AbstractType[] { DATA_TYPE, NATIVE, FINAL, CONSTANT, OPEN_BLOCK, FUNC };
	};
    }

    @Override
    public String toString() {
	return symbol;
    }

    // STATIC METHODS

    /**
     * Orders a list of flags, removes the duplicates and unnecessary ones.
     * 
     * Gets called in {@link Formatter#orderFlags}
     */
    public static List<String> orderFlags(List<String> flags) {
	// Remove duplicates
	flags = new ArrayList<String>(new HashSet<String>(flags));
	// Remove unnecessary.
	if (flags.contains(FINAL.symbol) && flags.contains(CONSTANT.symbol))
	    flags.remove(flags.indexOf(FINAL.symbol));

	// Order
	flags.sort(new Comparator<String>() {
	    @Override
	    public int compare(String o1, String o2) {
		FlagType f1 = fromString(o1, FlagType.class);
		FlagType f2 = fromString(o2, FlagType.class);
		return Integer.compare(f1.rank, f2.rank);
	    }
	});
	return flags;
    }
}
