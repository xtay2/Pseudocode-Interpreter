package expressions.normal;

import parsing.program.ExpressionType;
import parsing.program.KeywordType;

/**
 * Flags like native or static.
 */
public class Flag extends Expression {

	public enum FlagType {
		
		NATIVE(KeywordType.NATIVE);
		
		final KeywordType keyword;
		
		private FlagType(KeywordType keyword) {
			this.keyword = keyword;
		}
		
		@Override
		public String toString() {
			return keyword.toString();
		}
	}
	
	public final FlagType flagType;

	public Flag(FlagType flagType, int lineID) {
		super(lineID);
		setExpectedExpressions(ExpressionType.KEYWORD);
		this.flagType = flagType;
	}
}
