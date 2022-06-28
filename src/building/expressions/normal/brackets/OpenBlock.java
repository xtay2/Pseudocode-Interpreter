package building.expressions.normal.brackets;

import static building.types.specific.BuilderType.*;

import java.util.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.main.*;
import building.expressions.normal.*;
import launching.*;

public final class OpenBlock extends Expression implements BlockBracket {
	
	/** The lineID of the matching {@link CloseBlock} */
	private final int myMatch;
	
	public OpenBlock(int lineID) {
		super(lineID, OPEN_BLOCK);
		long brack = 1;
		for (int i = lineIdentifier + 1; i < Main.PROGRAM.size(); i++) {
			List<BuilderExpression> exp = Main.PROGRAM.getLine(i).getExpressions();
			for (BuilderExpression e : exp) {
				if (e.is(OPEN_BLOCK))
					brack++;
				if (e.is(CLOSE_BLOCK)) {
					brack--;
					if (brack == 0) {
						myMatch = e.lineIdentifier;
						return;
					}
				}
			}
		}
		throw new AssertionError("Found no matching CloseBlock.");
	}
	
	@Override
	public int getMatch() { return myMatch; }
}
