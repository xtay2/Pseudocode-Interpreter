package parser.program;

import expressions.main.functions.Function;
import expressions.main.loops.ForEachLoop;
import expressions.main.loops.FromToLoop;
import expressions.main.loops.WhileLoop;
import expressions.main.statements.IfStatement;
import expressions.main.statements.RepeatStatement;
import expressions.normal.Variable;

public enum KeywordType {

	/** {@link Variable} */
	VAR("var"),
	/** {@link Function} */
	FUNC("func"), MAIN("main"), RETURN("return"),
	/** {@link IfStatement} */
	IF("if"),
	/** {@link RepeatStatement} */
	REPEAT("repeat"),
	/** {@link WhileLoop} */
	WHILE("while"),
	/** {@link FromToLoop} */
	FROM("from"),
	/** {@link ForEachLoop} */
	FOR("for");

	public final String keyword;

	KeywordType(final String string) {
		keyword = string;
	}
}
