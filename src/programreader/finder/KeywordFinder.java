package programreader.finder;

import extensions.datastructures.sortedlist.SortedList;
import programreader.expressions.main.functions.Function;
import programreader.expressions.main.functions.MainFunction;
import programreader.expressions.main.statements.IfStatement;
import programreader.expressions.main.statements.RepeatStatement;
import programreader.expressions.main.statements.ReturnStatement;
import programreader.expressions.normal.Variable;
import programreader.expressions.special.Expression;
import programreader.program.KeywordType;

public class KeywordFinder {

	public static final SortedList<String> KEYWORDS = keywordsAsStrings();

	public static final int MAX_KEYWORD_LENGTH = getMaxKeywordLength();

	private static final SortedList<String> keywordsAsStrings() {
		SortedList<String> keywords = new SortedList<>();
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
		if (KeywordType.VAR.keyword.equals(arg))
			return new Variable(line);
		if (KeywordType.FUNC.keyword.equals(arg))
			return new Function(line);
		if(KeywordType.MAIN.keyword.equals(arg))
			return new MainFunction(line);
		if(KeywordType.RETURN.keyword.equals(arg))
			return new ReturnStatement(line);
		if(KeywordType.IF.keyword.equals(arg))
			return new IfStatement(line);
		if(KeywordType.REPEAT.keyword.equals(arg))
			return new RepeatStatement(line);
		throw new AssertionError("Keyword must be known by now. Was " + arg);
	}
}
