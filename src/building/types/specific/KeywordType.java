package building.types.specific;

import static building.types.abstractions.SuperType.DATA_TYPE;
import static building.types.abstractions.SuperType.VAL_HOLDER_TYPE;
import static building.types.specific.BuilderType.OPEN_BLOCK;
import static building.types.specific.DynamicType.NAME;
import static building.types.specific.operators.PrefixOpType.NOT;

import building.types.abstractions.AbstractType;
import building.types.abstractions.SpecificType;

/**
 * Specifies all Keywords and their text-representations. This includes non-functional keywords like
 * include and flags like native.
 *
 */
public enum KeywordType implements SpecificType {

	FROM("from"),

	REPEAT("repeat"),

	UNTIL("until"),

	WHILE("while"),

	IF("if"),

	ELIF("elif"),

	RETURN("return"),

	ANY("any"),

	ELSE("else"),

	MAIN("main"),

	FOR("for"),

	FUNC("func"),

	IMPORT("import"),

	IS("is");

	public final String symbol;

	/**
	 * Defines a BuilderType
	 * 
	 * @param id       is the unique identifying symbol from the code.
	 * @param expected are the expected following types. BuilderTypes allways expect themselves as
	 *                 followups.
	 */
	private KeywordType(String keyword) {
		symbol = keyword;
	}

	@Override
	public AbstractType[] abstractExpected() {
		return switch (this) {
			case FROM, UNTIL, WHILE, IF, ELIF, RETURN -> new AbstractType[] { VAL_HOLDER_TYPE };
			case REPEAT -> new AbstractType[] { VAL_HOLDER_TYPE, OPEN_BLOCK };
			case ANY -> new AbstractType[] { OPEN_BLOCK, IF };
			case ELSE, MAIN -> new AbstractType[] { OPEN_BLOCK };
			case FOR, FUNC -> new AbstractType[] { NAME };
			case IS -> new AbstractType[] { DATA_TYPE, NOT };
			case IMPORT -> throw new UnsupportedOperationException("An import Statement cannot be build.");
		};
	}

	@Override
	public String toString() {
		return symbol;
	}
}