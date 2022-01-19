package parsing.program;

import expressions.main.functions.Function;
import expressions.main.loops.ForEachLoop;
import expressions.main.loops.FromToLoop;
import expressions.main.statements.ElifStatement;
import expressions.main.statements.ElseStatement;
import expressions.main.statements.IfStatement;
import expressions.main.statements.RepeatStatement;
import parsing.importer.Importer;

public enum KeywordType {
	ELIF("elif"), ELSE("else"), /** {@link ForEachLoop} */
	FOR("for"),
	/** {@link FromToLoop} */
	FROM("from"), /** {@link Function} */
	FUNC("func"), /** {@link IfStatement}, {@link ElifStatement}, {@link ElseStatement} */
	IF("if"),
	/** {@link Importer} */
	IMPORT("import"),
	MAIN("main"),
	/** {@link NativeFunction} */
	NATIVE("native"),
	/** {@link RepeatStatement} */
	REPEAT("repeat"),
	RETURN("return"),
	/** {@link WhileLoop} */
	UNTIL("until"),
	/** {@link WhileLoop} */
	WHILE("while");

	public static KeywordType getKeywordFromString(String val) {
		for (KeywordType k : KeywordType.values()) {
			if (k.keyword.equals(val))
				return k;
		}
		return null;
	}

	public final String keyword;

	KeywordType(final String string) {
		keyword = string;
	}
}
