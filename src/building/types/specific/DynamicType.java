package building.types.specific;

import static building.types.abstractions.SuperType.*;
import static building.types.specific.BuilderType.*;
import static misc.util.Regex.*;

import building.expressions.main.blueprints.*;
import building.expressions.normal.*;
import building.expressions.normal.containers.*;
import building.expressions.normal.containers.name.*;
import building.types.abstractions.*;
import interpreting.program.*;

/**
 * Every type that the user can dynamically specify.
 */
public enum DynamicType implements SpecificType {
	
	/** {@link Literal} */
	LITERAL,
	
	/** {@link Name} */
	NAME,
	
	/** {@link Blueprint} */
	STATIC_LINK;
	
	@Override
	public BuilderExpression create(String arg, int lineID) {
		if (!switch (this) {
			case LITERAL -> ValueBuilder.isLiteral(arg);
			case NAME -> WR.matches(arg);
			case STATIC_LINK -> BP.matches(arg);
		}) { return null; }
		// Calls the specialised Constructor.
		return new BuilderExpression(lineID, this, arg);
	}
	
	@Override
	public AbstractType[] abstractExpected() {
		return switch (this) {
			case NAME -> new AbstractType[] {AFTER_VALUE_TYPE, ASSIGNMENT_TYPE, OPEN_BRACKET, ARRAY_START};
			case LITERAL -> new AbstractType[] {AFTER_VALUE_TYPE, RANGE};
			case STATIC_LINK -> new AbstractType[] {NAME};
		};
	}
	
	@Override
	public String toString() {
		return name().toLowerCase();
	}
}
