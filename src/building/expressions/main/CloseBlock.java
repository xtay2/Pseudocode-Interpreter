package building.expressions.main;

import static building.types.specific.BuilderType.*;

import java.util.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.normal.brackets.*;
import building.types.abstractions.*;
import building.types.specific.*;
import launching.*;

public final class CloseBlock extends MainExpression implements BlockBracket {
	
	/** The lineID of the matching {@link OpenBlock} */
	private final int myMatch;
	
	public CloseBlock(int lineID) {
		super(lineID, CLOSE_BLOCK);
		long brack = -1;
		for (int i = lineIdentifier - 1; i >= 0; i--) {
			MainExpression exp = Main.PROGRAM.getLine(i).getMainExpression();
			if (exp instanceof CloseBlock)
				brack--;
			if (exp instanceof BlockHolder) {
				brack++;
				if (brack == 0) {
					myMatch = exp.lineIdentifier;
					return;
				}
			}
		}
		throw new AssertionError("Found no matching OpenBlock.");
	}
	
	@Override
	public boolean execute() {
		return true; // Just go back
	}
	
	@Override
	public int getMatch() { return myMatch; }
	
	/**
	 * Returns an immutable list of all types that are allowed to follow a {@link CloseBlock} in code.
	 */
	public static List<SpecificType> allowedAfter() {
		return List.of(KeywordType.ELIF, KeywordType.ANY, KeywordType.ELSE, BuilderType.MULTI_CLOSE_SCOPE);
	}
}
