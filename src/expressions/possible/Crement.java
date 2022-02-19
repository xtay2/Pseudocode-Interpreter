package expressions.possible;

import static datatypes.numerical.NumberValue.ONE;
import static helper.Output.print;
import static types.specific.BuilderType.ARRAY_END;
import static types.specific.BuilderType.CLOSE_BRACKET;
import static types.specific.BuilderType.COMMA;
import static types.specific.ExpressionType.DECREMENT;
import static types.specific.ExpressionType.INCREMENT;
import static types.specific.ExpressionType.NAME;

import datatypes.TextValue;
import datatypes.Value;
import datatypes.numerical.DecimalValue;
import datatypes.numerical.NumberValue;
import exceptions.runtime.CastingException;
import expressions.abstractions.Expression;
import expressions.abstractions.PossibleMainExpression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import modules.parser.program.ValueMerger;
import types.specific.ExpressionType;

/**
 * Pre- or Post- In- or Decrement.
 */
public class Crement extends PossibleMainExpression implements ValueHolder, MergedExpression {

	/** Defines a Pre_X_crement or Post_X_crement. */
	private enum Position {
		POST, PRE;
	}

	private ValueChanger target;
	private Position pos;

	/**
	 * Constructs a new {@link Crement}.
	 * 
	 * @param type has to be {@link ExpressionType#INCREMENT} or {@link ExpressionType#DECREMENT}.
	 */
	public Crement(ExpressionType type, int lineID) {
		super(lineID, type, NAME, CLOSE_BRACKET, COMMA, ARRAY_END);
		if (type != INCREMENT && type != DECREMENT)
			throw new AssertionError("The ExpressionType has to be Increment or Decrement.");
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
		if (pure instanceof DecimalValue || (pure instanceof TextValue txt && Value.isNumber(txt.value))) {
			NumberValue edited = pure.asNumber();
			// Operation
			edited = (is(INCREMENT)) ? edited.add(ONE) : edited.sub(ONE);
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
