package expressions.possible;

import static helper.Output.print;
import static parsing.program.ExpressionType.ARRAY_END;
import static parsing.program.ExpressionType.CLOSE_BRACKET;
import static parsing.program.ExpressionType.COMMA;
import static parsing.program.ExpressionType.DEFINITE_LINEBREAK;
import static parsing.program.ExpressionType.NAME;

import datatypes.NumberValue;
import datatypes.TextValue;
import datatypes.Value;
import exceptions.runtime.CastingException;
import expressions.normal.Name;
import expressions.normal.Semicolon;
import expressions.normal.Variable;
import expressions.special.Expression;
import expressions.special.PossibleMainExpression;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.VarManager;

/**
 * Pre- or Post- In- or Decrement.
 */
public class Crement extends PossibleMainExpression implements ValueHolder{

	/** Defines a Increment or Decrement. */
	public enum Change {
		DEC, INC;
	}

	/** Defines a Pre_X_crement or Post_X_crement. */
	enum Position {
		POST, PRE;
	}

	public final Change change;
	private Name name;
	private Position pos;

	public Crement(Change change, int line) {
		super(line);
		setExpectedExpressions(NAME, DEFINITE_LINEBREAK, CLOSE_BRACKET, COMMA, ARRAY_END);
		this.change = change;
	}

	/**
	 * Just executes the in- or decrement without returning the value.
	 */
	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Executing " + toString());
		getValue();
		return callNextLine(doExecuteNext);
	}

	/**
	 * Performs the in- or decrement and returns the appropriate value.
	 */
	@Override
	public Value getValue() {
		Variable var = VarManager.get(name.getName(), getOriginalLine());
		Value pure = var.getValue();
		if (pure instanceof NumberValue || (pure instanceof TextValue txt && Value.isNumber(txt.rawString()))) {
			NumberValue edited = pure.asNumber();
			// Increment
			if (change == Change.INC)
				edited = NumberValue.add(edited, new NumberValue(1));
			// Decrement
			else
				edited = NumberValue.sub(edited, new NumberValue(1));
			var.setValue(edited);
			if (pos == Position.PRE)
				return edited;
			return pure;
		}
		throw new CastingException(getOriginalLine(), "");
	}

	/**
	 * <pre>
	 * Initialises this Crement with either: 
	 * [{@link Crement}], [{@link Name}], (optional {@link Semicolon})
	 * or 
	 * [{@link Name}], [{@link Crement}], (optional {@link Semicolon})
	 * </pre>
	 */
	@Override
	public void init(Name n, Expression... args) {
		if (n == null)
			throw new AssertionError(this + " has to be called on a name.");
		name = n;
		if(args.length != 0)
			throw new AssertionError("There shouldn't be args for " + this);
	}

	@Override
	public String toString() {
		String type = pos + "_" + change;
		if (Output.DEBUG)
			return this.getClass().getSimpleName();
		return name == null ? type : (pos == Position.PRE ? type + "_" + name.getName() : name.getName() + "_" + type);
	}

}
