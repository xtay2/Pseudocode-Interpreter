package runtime.datatypes.numerical;

import static building.types.specific.data.DataType.NUMBER;

/**
 * This class contains the conceptual constants {@link ConceptualNrValue#POS_INF},
 * {@link ConceptualNrValue#NEG_INF} and {@link ConceptualNrValue#NAN}.
 * 
 * Both infinite-values can be checked with {@link NumberValue#isInfinite()}. They each represent
 * the concept of an infinitly big or small number with nothing more extrem following after. This
 * means, even when multiplied or added to, infinity is always the same infinite value.
 * 
 * Everything that has to do with limes is not a number (NaN), because it is in the finite range,
 * but neither display- or countable.
 * 
 * For example: 1 / INF != 0
 */
public abstract class ConceptualNrValue extends NumberValue {

	/** Text-Representation of this {@link ConceptualNrValue}. */
	public final String txt;

	/**
	 * Private constructor for the constants {@link ConceptualNrValue#POS_INF},
	 * {@link ConceptualNrValue#NEG_INF} and {@link ConceptualNrValue#NAN}.
	 * 
	 * @param txt is their text-representation.
	 */
	private ConceptualNrValue(String txt) {
		super(NUMBER);
		this.txt = txt;
	}

	/** Positive infinity. */
	public static final ConceptualNrValue POS_INF = new ConceptualNrValue("INF") {

		@Override
		public NumberValue add(NumberValue v) {
			return v == NAN || v == NEG_INF ? NAN : POS_INF;
		}

		@Override
		public NumberValue sub(NumberValue v) {
			return v == NAN || v == POS_INF ? NAN : POS_INF;
		}

		@Override
		public NumberValue mult(NumberValue v) {
			if (v == NAN || v.equals(ZERO))
				return NAN;
			return v.isPositive() ? POS_INF : NEG_INF;
		}

		@Override
		public NumberValue div(NumberValue v) {
			if (v == NAN || v.equals(ZERO) || v.isInfinite())
				return NAN;
			return v.isPositive() ? POS_INF : NEG_INF;
		}

		@Override
		public NumberValue mod(NumberValue v) {
			return NAN;
		}

		@Override
		public NumberValue pow(NumberValue v) {
			if (v.equals(ZERO))
				return ONE;
			return v == NAN || v == NEG_INF ? NAN : POS_INF;
		}

		@Override
		public NumberValue root(NumberValue v) {
			if (v.equals(ZERO))
				return ZERO;
			return NAN;
		}

		@Override
		public Double raw() {
			return Double.POSITIVE_INFINITY;
		}

	};

	/** Negative infinity. */
	public static final ConceptualNrValue NEG_INF = new ConceptualNrValue("-INF") {

		@Override
		public NumberValue add(NumberValue v) {
			return v == NAN || v == POS_INF ? NAN : NEG_INF;
		}

		@Override
		public NumberValue sub(NumberValue v) {
			return v == NAN || v == NEG_INF ? NAN : NEG_INF;
		}

		@Override
		public NumberValue mult(NumberValue v) {
			if (v == NAN || v.equals(ZERO))
				return NAN;
			return v.isPositive() ? NEG_INF : POS_INF;
		}

		@Override
		public NumberValue div(NumberValue v) {
			if (v == NAN || v.equals(ZERO) || v.isInfinite())
				return NAN;
			return v.isPositive() ? NEG_INF : POS_INF;
		}

		@Override
		public NumberValue mod(NumberValue v) {
			return NAN;
		}

		@Override
		public NumberValue pow(NumberValue v) {
			if (v == NAN || v.isInfinite())
				return NAN;
			if (v.equals(ZERO))
				return ONE;
			if (v instanceof IntValue i)
				return i.isEven() ? POS_INF : NEG_INF;
			throw new ArithmeticException("Complex numbers are not supported yet.");
		}

		@Override
		public NumberValue root(NumberValue v) {
			return NAN;
		}

		@Override
		public Double raw() {
			return Double.NEGATIVE_INFINITY;
		}

	};

	/**
	 * Not a Number. Every limes-value or operation that fails or gets NaN as an input, should return
	 * this.
	 * 
	 * <pre>
	 * x / 0 = NaN
	 * x / INF = NaN
	 * INF - INF = NaN
	 * </pre>
	 */
	public static final ConceptualNrValue NAN = new ConceptualNrValue("NaN") {

		@Override
		public NumberValue add(NumberValue v) {
			return NAN;
		}

		@Override
		public NumberValue sub(NumberValue v) {
			return NAN;
		}

		@Override
		public NumberValue mult(NumberValue v) {
			return NAN;
		}

		@Override
		public NumberValue div(NumberValue v) {
			return NAN;
		}

		@Override
		public NumberValue mod(NumberValue v) {
			return NAN;
		}

		@Override
		public NumberValue pow(NumberValue v) {
			return NAN;
		}

		@Override
		public NumberValue root(NumberValue v) {
			return NAN;
		}

		@Override
		public Double raw() {
			return Double.NaN;
		}
	};
}
