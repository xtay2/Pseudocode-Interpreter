package expressions.special;

import expressions.main.CloseScope;
import expressions.main.MainExpression;
import expressions.normal.brackets.OpenScope;
import main.Main;

/**
 * {@link OpenScope} {@link CloseScope}
 */
public abstract class Scope extends MainExpression {

	protected OpenScope openScope = null;

	public Scope(int line) {
		super(line);
	}

	public abstract String getScopeName();

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

	public static final Scope GLOBAL_SCOPE = new Scope(-1) {

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