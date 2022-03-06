package building.types.specific;

import static building.types.SuperType.EXPECTED_TYPE;
import static building.types.SuperType.EXPRESSION_TYPE;
import static building.types.specific.BuilderType.ARRAY_START;
import static building.types.specific.BuilderType.OPEN_BRACKET;
import static building.types.specific.BuilderType.OPEN_SCOPE;
import static building.types.specific.ExpressionType.NAME;

import building.types.AbstractType;
import building.types.SuperType;

/**
 * Specifies all Keywords and their text-representations. This includes non-functional keywords like
 * include and flags like native.
 *
 */
public enum KeywordType implements AbstractType {

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

	public final String keyword;

	/**
	 * Defines a BuilderType
	 * 
	 * @param id       is the unique identifying symbol from the code.
	 * @param expected are the expected following types. BuilderTypes allways expect themselves as
	 *                 followups.
	 */
	private KeywordType(String keyword) {
		this.keyword = keyword;
	}

	@Override
	public String toString() {
		return keyword;
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.KEYWORD_TYPE;
	}

	/** Checks, if the passed {@link String} is a {@link KeywordType}. */
	public static boolean isKeyword(String arg) {
		for (KeywordType t : values()) {
			if (t.keyword.equals(arg))
				return true;
		}
		return false;
	}

	@Override
	public AbstractType[] expected() {
		return switch (this) {
			case FROM, UNTIL, WHILE, IF, ELIF, RETURN -> AbstractType.valHolderTypes();
			case REPEAT -> new AbstractType[] { EXPRESSION_TYPE, ARRAY_START, OPEN_BRACKET, OPEN_SCOPE };
			case ANY -> new AbstractType[] { OPEN_SCOPE, IF };
			case ELSE, MAIN -> new AbstractType[] { OPEN_SCOPE };
			case FOR, FUNC -> new AbstractType[] { NAME };
			case IS -> new AbstractType[] { EXPECTED_TYPE };
			case IMPORT -> throw new UnsupportedOperationException("An import Statement cannot be build.");
		};
	}
}