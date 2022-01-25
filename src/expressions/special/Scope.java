package expressions.special;

import expressions.main.CloseScope;
import expressions.main.MainExpression;
import expressions.normal.brackets.OpenScope;
import main.Main;

/**
 * {@link OpenScope}
 * {@link CloseScope}
 */
public abstract class Scope extends MainExpression {

	protected OpenScope block = null;

	public Scope(int line) {
		super(line);
	}

	public abstract String getScopeName();

	public int getStart() {
		return lineIdentifier;
	}

	public int getEnd() {
		try {
			return ((CloseScope) block.getMatch()).lineIdentifier + 1;
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
		block.setMyMatch(cs);
		cs.setMyMatch(block);
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