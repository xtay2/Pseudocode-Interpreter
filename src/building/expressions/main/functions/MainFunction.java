package building.expressions.main.functions;

import static building.types.specific.FlagType.*;

import java.util.*;

import building.expressions.abstractions.interfaces.*;
import building.expressions.abstractions.scopes.*;
import building.expressions.normal.brackets.*;
import building.expressions.normal.containers.name.*;
import building.types.specific.*;
import runtime.datatypes.*;

public class MainFunction extends Definition {
	
	/**
	 * Creates a {@link MainFunction} and registers it in the outer {@link Scope}.
	 *
	 * @param os is the {@link OpenBlock} of this {@link ScopeHolder}. Shouldn't be null.
	 */
	public MainFunction(int lineID, OpenBlock os) {
		super(lineID, (VarName) Name.generateName(lineID, KeywordType.MAIN.toString()), null, os);
		addFlags(Set.of(FINAL));
	}
	
	@Override
	public Value call(ValueHolder... params) {
		callFirstLine();
		return null;
	}
	
	@Override
	public int expectedParams() {
		return 0;
	}
}
