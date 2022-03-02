package modules.merger;

import static helper.Output.print;

import java.util.Collections;
import java.util.List;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.abstractions.Expression;
import expressions.abstractions.GlobalScope;
import expressions.abstractions.MainExpression;
import expressions.abstractions.Scope;
import expressions.abstractions.ScopeHolder;
import expressions.main.CloseScope;
import expressions.normal.BuilderExpression;
import main.Main;
import modules.parser.program.ProgramLine;

public abstract class ExpressionMerger {

	protected static List<BuilderExpression> line;
	protected static List<BuilderExpression> orgExp;
	protected static int lineID;
	protected static int orgLine;

	/**
	 * Takes all pure {@link Expression}s from a {@link ProgramLine} as input and merges them into a
	 * {@link MainExpression}.
	 * 
	 * @param line
	 */
	public static MainExpression merge(ProgramLine pline) {
		// Init
		orgExp = Collections.unmodifiableList(pline.getExpressions());
		line = pline.getExpressions();
		lineID = pline.lineID;
		orgLine = pline.orgLine;
		final Scope scope = findScope();
		// Merge
		print("Merging " + orgLine + " in " + scope.getScopeName() + ": \t" + orgExp);
		MainExpression main;
		try {
			main = (MainExpression) SuperMerger.build();
		} catch (Exception | Error e) {
			e.printStackTrace();
			System.err.print("\nCaused: ");
			throw new IllegalCodeFormatException(orgLine,
					"Unknown unpropper format." + "\nOriginal state of line " + orgExp + "\nCurrent state of line: " + line);
		}
		// Set the Scope for a fully merged Expression.
		main.setScope(scope);
		// Initialise the own Scope it this main is a ScopeHolder.
		if (main instanceof ScopeHolder sh)
			sh.initScope();
		// Check if line was correctly build
		if (main == null || !SuperMerger.line.isEmpty()) {
			throw new AssertionError(
					"Main-Merge got finished too early or was null.\nMain: " + main + "\nOriginal Line:" + orgLine + "\nLine: " + line);
		}
		return main;
	}

	/** Finds the Scope of this line. */
	private static Scope findScope() {
		if (lineID == 0)
			return GlobalScope.GLOBAL;
		MainExpression m = Main.PROGRAM.getLine(lineID - 1).getMainExpression();
		if (m instanceof CloseScope c) {
			// Saves the ScopeHolder in which this line lies.
			ScopeHolder match = (ScopeHolder) Main.PROGRAM.getLine(c.getMatch()).getMainExpression();
			return match.getOuterScope();
		}
		return m.getScope();
	}

}
