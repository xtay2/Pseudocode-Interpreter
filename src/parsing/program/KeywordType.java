package parsing.program;

import expressions.main.functions.Function;
import expressions.main.loops.ForEachLoop;
import expressions.main.loops.FromToLoop;
import expressions.main.loops.WhileLoop;
import expressions.main.statements.IfStatement;
import expressions.main.statements.RepeatStatement;
import parsing.importer.Importer;

public enum KeywordType {
	/** {@link Function} */
	FUNC("func"), MAIN("main"), RETURN("return"),
	/** {@link IfStatement}, {@link ElifStatement}, {@link ElseStatement} */
	IF("if"), ELIF("elif"), ELSE("else"),
	/** {@link RepeatStatement} */
	REPEAT("repeat"),
	/** {@link WhileLoop} */
	WHILE("while"),
	/** {@link FromToLoop} */
	FROM("from"),
	/** {@link ForEachLoop} */
	FOR("for"),
	/** {@link Importer} */
	IMPORT("import"),
	/** {@link NativeFunction} */
	NATIVE("native");

	public final String keyword;

	public static KeywordType getKeywordFromString(String val) {
		for (KeywordType k : KeywordType.values()) {
			if (k.keyword.equals(val))
				return k;
		}
		return null;
	}

	KeywordType(final String string) {
		keyword = string;
	}
}
