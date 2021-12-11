package parser.finder;

import java.util.ArrayList;

import expressions.main.functions.Function;
import expressions.main.functions.MainFunction;
import expressions.main.loops.ForEachLoop;
import expressions.main.loops.FromToLoop;
import expressions.main.loops.WhileLoop;
import expressions.main.statements.ElifStatement;
import expressions.main.statements.ElseStatement;
import expressions.main.statements.IfStatement;
import expressions.main.statements.RepeatStatement;
import expressions.main.statements.ReturnStatement;
import expressions.normal.Variable;
import expressions.special.Expression;
import parser.program.KeywordType;

public class KeywordFinder {

	public static final ArrayList<String> KEYWORDS = keywordsAsStrings();

	public static final int MAX_KEYWORD_LENGTH = getMaxKeywordLength();

	private static final ArrayList<String> keywordsAsStrings() {
		ArrayList<String> keywords = new ArrayList<>();
		for (KeywordType k : KeywordType.values())
			keywords.add(k.keyword);
		return keywords;
	}

	private static int getMaxKeywordLength() {
		int longest = -1;
		for (KeywordType k : KeywordType.values())
			longest = Math.max(longest, k.keyword.length());
		return longest;
	}

	public static boolean isKeyword(String string) {
		return KEYWORDS.contains(string);
	}

	public static Expression keywordExpression(String arg, int line) {	
		if (KeywordType.FUNC.keyword.equals(arg))
			return new Function(line);
		if (KeywordType.MAIN.keyword.equals(arg))
			return new MainFunction(line);
		if (KeywordType.RETURN.keyword.equals(arg))
			return new ReturnStatement(line);
		if (KeywordType.IF.keyword.equals(arg))
			return new IfStatement(line);
		if (KeywordType.ELIF.keyword.equals(arg))
			return new ElifStatement(line);
		if (KeywordType.ELSE.keyword.equals(arg))
			return new ElseStatement(line);
		if (KeywordType.REPEAT.keyword.equals(arg))
			return new RepeatStatement(line);
		if (KeywordType.WHILE.keyword.equals(arg))
			return new WhileLoop(line);
		if (KeywordType.FROM.keyword.equals(arg))
			return new FromToLoop(line);
		if (KeywordType.FOR.keyword.equals(arg))
			return new ForEachLoop(line);
		throw new AssertionError("Keyword must be known by now. Was " + arg);
	}
}
