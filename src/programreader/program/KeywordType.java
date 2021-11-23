package programreader.program;

import programreader.expressions.main.functions.Function;
import programreader.expressions.main.functions.MainFunction;
import programreader.expressions.main.statements.RepeatStatement;
import programreader.expressions.main.statements.ReturnStatement;
import programreader.expressions.normal.Variable;

public enum KeywordType {

	/**
	 * @see Variable
	 * @see Function
	 * @see MainFunction
	 * @see ReturnStatement
	 * @see IfStatement
	 * @see RepeatStatement
	 */
	VAR("var"), FUNC("func"), MAIN("main"), RETURN("return"), IF("if"), REPEAT("repeat");

	public final String keyword;

	KeywordType(final String string) {
		keyword = string;
	}
}
