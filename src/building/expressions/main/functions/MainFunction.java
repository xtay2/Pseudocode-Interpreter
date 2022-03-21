package building.expressions.main.functions;

import static building.types.specific.KeywordType.MAIN;

import building.expressions.abstractions.Scope;
import building.expressions.abstractions.ScopeHolder;
import building.expressions.abstractions.interfaces.Callable;
import building.expressions.abstractions.interfaces.Registerable;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.types.specific.KeywordType;
import runtime.datatypes.Value;

public class MainFunction extends ScopeHolder implements Registerable, Callable {

	public final Name name;

	/**
	 * Creates a {@link MainFunction} and registers it in the outer {@link Scope}.
	 * 
	 * @param os is the {@link OpenBlock} of this {@link ScopeHolder}. Shouldn't be null.
	 */
	public MainFunction(int lineID, OpenBlock os) {
		super(lineID, MAIN, os);
		this.name = new Name(lineIdentifier, KeywordType.MAIN.toString());
	}

	@Override
	public boolean execute() {
		throw new AssertionError("A main-declaration cannot be executed.");
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
	public Name getName() {
		return name;
	}

}
