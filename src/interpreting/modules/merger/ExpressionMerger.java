package interpreting.modules.merger;

import static misc.helper.Output.print;

import java.util.Collections;
import java.util.List;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.GlobalScope;
import building.expressions.abstractions.MainExpression;
import building.expressions.abstractions.Scope;
import building.expressions.abstractions.ScopeHolder;
import building.expressions.main.CloseScope;
import building.expressions.normal.BuilderExpression;
import building.types.AbstractType;
import building.types.specific.BuilderType;
import building.types.specific.FlagType;
import building.types.specific.KeywordType;
import interpreting.exceptions.IllegalCodeFormatException;
import interpreting.program.ProgramLine;
import misc.main.Main;

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
		debugLine(scope);
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

	/**
	 * Constructs an {@link Expression} from the {@link AbstractType} of the first
	 * {@link BuilderExpression}.
	 */
	protected static Expression build() {
		BuilderExpression fst = line.get(0);
		// Build the right MainExpression through recursive pattern matching.
		Expression result = switch (fst.type) {
			case KeywordType k -> SuperMerger.buildKeyword();
			case BuilderType b -> SuperMerger.buildAbstract();
			case FlagType f -> (Expression) SuperMerger.buildFlaggable();
			default -> (Expression) SuperMerger.buildVal();
		};
		return result;
	}

	private static void debugLine(Scope scope) {
		// Merge
		String ls = "Merging " + orgLine + " in " + scope.getScopeName() + ": ";
		final int MAX = 30;
		if (ls.length() > MAX)
			print(ls + " \t" + orgExp);
		else
			print(ls + " ".repeat(MAX - ls.length()) + orgExp);
	}
}
