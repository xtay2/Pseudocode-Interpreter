package interpreting.modules.merger;

import java.util.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.abstractions.scopes.*;
import building.expressions.main.*;
import building.expressions.main.blueprints.*;
import building.expressions.main.functions.*;
import building.expressions.main.statements.*;
import building.expressions.normal.*;
import building.types.abstractions.*;
import building.types.specific.*;
import errorhandeling.*;
import importing.filedata.paths.*;
import interpreting.program.*;
import launching.*;

public abstract class ExpressionMerger {
	
	protected static List<BuilderExpression> line;
	protected static List<BuilderExpression> orgExp;
	protected static int lineID;
	
	/**
	 * This is a {@link BlueprintPath} for every line that doesn't contain a {@link Blueprint}. (Should
	 * be used for exceptions, can get casted for {@link Definition}s.)
	 */
	protected static DataPath path;
	
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
		path = pline.getDataPath();
		MainExpression main = (MainExpression) SuperMerger.build();
		// Check if line was correctly build
		if (main == null || !SuperMerger.line.isEmpty()) {
			throw new AssertionError(
					path + ": Main-Merge got finished too early or was null.\nMain: " + main + "\nLine: " + line + "\nOrgLine: " + orgExp);
		}
		// Sets the Scope
		try {
			if (main instanceof Definition def)
				ScopeManager.DEFS.register(((BlueprintPath) path).blueprint, def);
		} catch (ClassCastException cce) {
			throw new PseudocodeException("NoBlueprint", "There is code outside of the blueprint.", path);
		}
		// Sets flags from overlying FlagSpaces
		if (main instanceof Flaggable f)
			collectFlags(f);
		return main;
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
			case BlueprintType b:
				yield SuperMerger.buildBlueprint();
			default:
				yield (Expression) SuperMerger.buildVal();
		};
		return result;
	}
}
