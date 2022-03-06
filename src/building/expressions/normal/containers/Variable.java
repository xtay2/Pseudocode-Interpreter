package building.expressions.normal.containers;

import static building.types.specific.FlagType.CONSTANT;
import static building.types.specific.FlagType.FINAL;
import static misc.helper.Output.print;
import static runtime.datatypes.object.NullValue.NULL;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.Scope;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.expressions.normal.flag.Flaggable;
import building.expressions.possible.Call;
import building.types.specific.FlagType;
import building.types.specific.data.ExpectedType;
import runtime.datatypes.Value;
import runtime.datatypes.object.NullValue;
import runtime.exceptions.CastingException;
import runtime.exceptions.DeclarationException;

/**
 * Has a Name and a Value. The Name has a scope.
 *
 * Gets created by keywords like var, bool, nr, text, obj or as a parameter in a function through
 * the {@link ExpectedType}.
 *
 * Gets saved in its {@link Scope} and should only get accessed by it.
 */
public class Variable extends Expression implements ValueChanger, Flaggable {

	// DATA
	private final Name name;
	private Value value;

	// FLAGS
	private final Set<FlagType> flags = new HashSet<>();

	/**
	 * Creates and registers a Variable.
	 * 
	 * @param lineID is lineID of the {@link Expression} in which this var gets created.
	 * @param scope  is the outer Scope in which this {@link Variable} lies.
	 * @param type   is the {@link ExpectedType} of this {@link Variable}.
	 * @param name   is the unique {@link Name} of this {@link Variable}.
	 * @param val    is an optional {@link Variable}. Input null if no value is wanted.
	 * @param flags  are optional {@link FlagType}s.
	 * @return the finished/registered {@link Variable}.
	 */
	public static Variable quickCreate(int lineID, Scope scope, ExpectedType type, Name name, Value val, FlagType... flags) {
		Variable v = new Variable(lineID, type, name, val);
		v.setFlags(Set.copyOf(Arrays.asList(flags)));
		scope.register(v);
		print("Created the " + type + " \"" + name.getNameString() + "\" in the scope " + scope.getScopeName());
		return v;
	}

	/**
	 * Initialise a Variable with an inital Value. Used in {@link Call} and {@link #quickCreate()}.
	 */
	private Variable(int lineID, ExpectedType type, Name name, Value val) {
		super(lineID, type);
		if (type == null)
			throw new AssertionError("The type cannot be null.");
		this.name = name;
		setValue(val);
	}

	/**
	 * Returns the {@link Value} of this variable or {@link NullValue#NULL} if it isn't initialised yet.
	 */
	@Override
	public Value getValue() {
		if (value == null)
			return NULL;
		return value;
	}

	/**
	 * Should get identified through {@link Scope#get()}.
	 * 
	 * @throws CastingException if this is a TypedVar and the types don't match.
	 */
	@Override
	public void setValue(Value val) throws CastingException {
		if (val == null)
			throw new AssertionError("Value cannot be null.");
		if (hasFlag(FINAL) || hasFlag(CONSTANT) && value != null)
			throw new DeclarationException(getOriginalLine(),
					"Trying to modify the " + (hasFlag(CONSTANT) ? "constant " : "final variable ") + getName());
		value = val.as((ExpectedType) type);
	}

	@Override
	public Name getName() {
		return name;
	}

	@Override
	public void setFlags(Set<FlagType> flags) {
		this.flags.addAll(flags);
	}

	@Override
	public boolean hasFlag(FlagType f) {
		return flags.contains(f);
	}
}
