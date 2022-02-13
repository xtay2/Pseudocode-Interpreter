package expressions.possible;

import static helper.Output.print;
import static parsing.program.ExpressionType.*;

import datatypes.NumberValue;
import datatypes.TextValue;
import datatypes.Value;
import exceptions.runtime.CastingException;
import expressions.abstractions.Expression;
import expressions.abstractions.MergedExpression;
import expressions.abstractions.PossibleMainExpression;
import expressions.abstractions.ValueChanger;
import expressions.abstractions.ValueHolder;
import parsing.program.ValueMerger;

/**
 * Pre- or Post- In- or Decrement.
 */
public class Crement extends PossibleMainExpression implements ValueHolder, MergedExpression {

	/** Defines a Increment or Decrement. */
	public enum Change {
		DEC, INC;
	}

	/** Defines a Pre_X_crement or Post_X_crement. */
	enum Position {
		POST, PRE;
	}

	public final Change change;
	private ValueChanger target;
	private Position pos;

	public Crement(Change change, int line) {
		super(line);
		setExpectedExpressions(NAME, CLOSE_BRACKET, COMMA, ARRAY_END);
		this.change = change;
	}

	/**
	 * Just executes the in- or decrement without returning the value.
	 */
	@Override
	public boolean execute(ValueHolder... params) {
		print("Executing " + toString());
		getValue();
		return callNextLine();
	}

	/**
	 * Performs the in- or decrement and returns the appropriate value.
	 */
	@Override
	public Value getValue() {
		Value pure = target.getValue();
		if (pure instanceof NumberValue || (pure instanceof TextValue txt && Value.isNumber(txt.rawString()))) {
			NumberValue edited = pure.asNumber();
			// Increment
			if (change == Change.INC)
				edited = NumberValue.add(edited, NumberValue.ONE);
			// Decrement
			else
				edited = NumberValue.sub(edited, NumberValue.ONE);
			target.setValue(edited);
			if (pos == Position.PRE)
				return edited;
			return pure;
		}
		throw new CastingException(getOriginalLine(), "Only numbers can get cremented.");
	}

	/**
	 * <pre>
	 * Initialises this Crement with either: 
	 * [{@link Crement}], [{@link ValueChanger}], (optional {@link Semicolon})
	 * or 
	 * [{@link ValueChanger}], [{@link Crement}], (optional {@link Semicolon})
	 * </pre>
	 * 
	 * Called in {@link ValueMerger}
	 */
	@Override
	public void merge(Expression... e) {
		if (e[0] instanceof ValueChanger n) {
			target = n;
			pos = Position.POST;
		} else if (e[1] instanceof ValueChanger n) {
			target = n;
			pos = Position.PRE;
		}
	}
}
