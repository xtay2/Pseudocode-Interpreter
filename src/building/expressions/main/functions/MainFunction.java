package building.expressions.main.functions;

import static building.types.specific.FlagType.FINAL;

import java.util.Set;

import building.expressions.abstractions.Scope;
import building.expressions.abstractions.ScopeHolder;
import building.expressions.abstractions.interfaces.Registerable;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.types.specific.KeywordType;
import runtime.datatypes.Value;

public class MainFunction extends Definition implements Registerable {

	/** Creates a {@link MainFunction} and registers it in the outer {@link Scope}.
	 * 
	 * @param os is the {@link OpenBlock} of this {@link ScopeHolder}. Shouldn't be null. */
	public MainFunction(int lineID, OpenBlock os) {
		super(lineID, new Name(lineID, KeywordType.MAIN.toString()), null, false, os);
		addFlags(Set.of(FINAL));
	}

	@Override
	public Value call(ValueHolder... params) {
		Scope.tos++;
		callFirstLine();
		getScope().clear();
		Scope.tos--;
		return null;
	}

	@Override
	public int expectedParams() {
		return 0;
	}
}
