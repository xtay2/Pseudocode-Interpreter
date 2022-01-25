package parsing.finder;

import java.util.ArrayList;

import parsing.program.KeywordType;

public class KeywordFinder {

	public static final ArrayList<String> KEYWORDS = keywordsAsStrings();

	public static final int MAX_KEYWORD_LENGTH = getMaxKeywordLength();

	private static int getMaxKeywordLength() {
		int longest = -1;
		for (KeywordType k : KeywordType.values())
			longest = Math.max(longest, k.toString().length());
		return longest;
	}

	public static boolean isKeyword(String string) {
		return KEYWORDS.contains(string);
	}

	private static final ArrayList<String> keywordsAsStrings() {
		ArrayList<String> keywords = new ArrayList<>();
		for (KeywordType k : KeywordType.values())
			keywords.add(k.toString());
		return keywords;
	}
}
