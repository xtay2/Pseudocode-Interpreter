package expressions.normal;

import expressions.special.Expression;
import helper.Output;
import parsing.program.ExpressionType;
import parsing.program.KeywordType;

public class Keyword extends Expression {

	private final String keyword;

	public Keyword(int line, String keyword) {
		super(line);
		setExpectedExpressions(ExpressionType.KEYWORD);
		this.keyword = keyword;
	}

	public KeywordType getKeyword() {
		return KeywordType.getKeywordFromString(keyword);
	}
	
	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : keyword;
	}

}
