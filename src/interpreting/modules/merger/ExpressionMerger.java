package interpreting.modules.merger;

import static misc.helper.Output.print;

import java.util.Collections;
import java.util.List;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.GlobalScope;
import building.expressions.abstractions.MainExpression;
import building.expressions.abstractions.Scope;
import building.expressions.abstractions.ScopeHolder;
import building.expressions.abstractions.interfaces.Flaggable;
import building.expressions.abstractions.interfaces.Registerable;
import building.expressions.main.CloseBlock;
import building.expressions.main.functions.MainFunction;
import building.expressions.main.functions.Definition;
import building.expressions.main.statements.FlagSpace;
import building.expressions.normal.BuilderExpression;
import building.types.AbstractType;
import building.types.specific.BuilderType;
import building.types.specific.FlagType;
import building.types.specific.KeywordType;
import interpreting.exceptions.IllegalCodeFormatException;
import interpreting.program.ProgramLine;
import misc.main.Main;
import runtime.defmanager.DefManager;

public abstract class ExpressionMerger {

	protected static List<BuilderExpression> line;
	protected static List<BuilderExpression> orgExp;
	protected static int lineID;
	protected static int orgLine;
	protected static Scope outer;

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
		outer = findScope();
		debugLine(outer);
		MainExpression main;
		try {
			main = (MainExpression) SuperMerger.build();
		} catch (Exception | Error e) {
			e.printStackTrace();
			System.err.print("\nCaused: ");
			throw new IllegalCodeFormatException(orgLine,
					"Unknown unpropper format." + "\nOriginal state of line " + orgExp + "\nCurrent state of line: " + line);
		}
		// Check if line was correctly build
		if (main == null || !SuperMerger.line.isEmpty()) {
			throw new AssertionError(
					"Main-Merge got finished too early or was null.\nMain: " + main + "\nOriginal Line:" + orgLine + "\nLine: " + line);
		}
		// Sets the Scope
		initScopes(main);
		// Sets flags from overlying FlagSpaces
		if (main instanceof Flaggable f)
			collectFlags(f);
		return main;
	}

	/**
	 * This function:
	 * 
	 * <pre>
	 * -Sets the {@link Scope} of the {@link MainExpression}.
	 * -Registers the inner {@link Scope} if the main is a {@link ScopeHolder}.
	 * -Registers the main, if it is a {@link Registerable} {@link Definition} or {@link MainFunction}.
	 * </pre>
	 */
	private static void initScopes(MainExpression main) {
		// Set the Scope for a fully merged Expression.
		main.setScope(outer);

		// Initialise the own Scope if this main is a ScopeHolder.
		if (main instanceof ScopeHolder sh)
			sh.initScope();

		// Register the main func immediatly.
		if (main instanceof MainFunction)
			((ScopeHolder) main).getOuterScope().register((Registerable) main);

		if (main instanceof Definition r)
			DefManager.register(r);
	}

	/**
	 * If the {@link MainExpression} is a {@link Flaggable}, and lies in a {@link FlagSpace}, it gets
	 * all of the flags from that {@link FlagSpace}.
	 */
	private static void collectFlags(Flaggable main) {
		for (int i = lineID - 1; i >= 0; i--) {
			MainExpression m = Main.PROGRAM.getLine(i).getMainExpression();
			if (m instanceof FlagSpace fs)
				main.addFlags(fs.getFlags());
			if (m instanceof CloseBlock cb)
				i = cb.getMatch();
		}
	}

	/** Finds the Scope of this line. */
	private static Scope findScope() {
		for (int i = lineID - 1; i >= 0; i--) {
			MainExpression m = Main.PROGRAM.getLine(i).getMainExpression();
			if (m instanceof ScopeHolder sh)
				return sh.getScope();
			if (m instanceof CloseBlock cb)
				i = cb.getMatch();
		}
		return GlobalScope.GLOBAL;
	}

	/**
	 * Constructs an {@link Expression} from the {@link AbstractType} of the first
	 * {@link BuilderExpression}.
	 */
	protected static Expression build() {
		BuilderExpression fst = line.get(0);
		// Build the right MainExpression through recursive pattern matching.
		Expression result = switch (fst.type) {
			case KeywordType k:
				yield SuperMerger.buildKeyword();
			case BuilderType b:
				yield SuperMerger.buildAbstract();
			case FlagType f:
				yield (Expression) SuperMerger.buildFlaggable();
			default:
				yield (Expression) SuperMerger.buildVal();
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
