package expressions.normal;

import static parsing.program.ExpressionType.KEYWORD;

import expressions.special.Expression;
import helper.Output;
import parsing.program.KeywordType;
public class Keyword extends Expression {

	private final String keyword;

	public Keyword(int line, String keyword) {
		super(line);
		setExpectedExpressions(KEYWORD);
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
