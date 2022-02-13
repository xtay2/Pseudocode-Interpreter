package expressions.abstractions;

import expressions.main.CloseScope;
import expressions.normal.brackets.OpenScope;
import main.Main;
import parsing.program.ExpressionType;
import parsing.program.KeywordType;

/**
 * {@link OpenScope} {@link CloseScope}
 */
public abstract class Scope extends MainExpression {

	protected OpenScope openScope = null;

	public abstract String getScopeName();

	public Scope(int line, KeywordType myKeyword) {
		super(line, myKeyword);
	}

	public Scope(int line, ExpressionType myType) {
		super(line, myType);
	}
	
	public int getStart() {
		return openScope.lineIdentifier;
	}

	/**
	 * Returns the lineIdentifier of the matching {@link CloseScope}.
	 */
	public int getEnd() {
		try {
			return ((CloseScope) openScope.getMatch()).lineIdentifier + 1;
		} catch (NullPointerException e) {
			throw new AssertionError("The scope " + this + " doesn't get closed. Use a ; or a }.");
		}
	}

	/**
	 * Gets called by {@link CloseScope} after construction.
	 * 
	 * @param cs is the closing scope bracket, that searches for its partner.
	 */
	public final void connectScopeEnd(CloseScope cs) {
		openScope.setMyMatch(cs);
		cs.setMyMatch(openScope);
	}

	public static final Scope GLOBAL_SCOPE = new Scope(-1, ExpressionType.MERGED) {

		@Override
		public boolean execute(ValueHolder... params) {
			throw new AssertionError("The global scope should not be treated like an expression.");
		}

		@Override
		public int getEnd() {
			return Main.PROGRAM.size();
		}

		@Override
		public String getScopeName() {
			return "global";
		}

		@Override
		public int getStart() {
			return 0;
		}
	};
}