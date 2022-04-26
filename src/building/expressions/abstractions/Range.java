package building.expressions.abstractions;

import building.types.specific.BuilderType;

/** A range-helper class for array-lengths. */
public class Range {

	/** An array with "unlimited" size. */
	public static final Range UNBOUNDED = new Range(0, Integer.MAX_VALUE);

	public final int lowerBound, upperBound;

	public static Range unbounded() {
		return UNBOUNDED;
	}

	/** Creates an Range that should contain a minimum of n items.
	 * 
	 * var[n..] */
	public static Range lowerBound(int bound) {
		return new Range(bound, Integer.MAX_VALUE);
	}

	/** Creates an Range that should contain a maximum of n items.
	 * 
	 * var[..n] */
	public static Range upperBound(int bound) {
		return new Range(0, bound);
	}

	/** Creates an Range that should exactly n items.
	 * 
	 * var[n] */
	public static Range exact(int exact) {
		return new Range(exact, exact);
	}

	/** Creates an Range that should contain between n and m items.
	 * 
	 * var[n..m] */
	public static Range intervalBound(int lowerBound, int upperBound) {
		return new Range(lowerBound, upperBound);
	}

	private Range(int lower, int upper) {
		if (lower > upper)
			throw new IllegalArgumentException("The upper bound has to be higher than the upper bound.");
		this.lowerBound = lower;
		this.upperBound = upper;
	}

	@Override
	public boolean equals(Object obj) {
		return obj != null && obj instanceof Range r && lowerBound == r.lowerBound && upperBound == r.upperBound;
	}

	@Override
	public String toString() {
		if (lowerBound == upperBound)
			return String.valueOf(lowerBound);
		String lower = lowerBound == 0 ? "" : String.valueOf(lowerBound);
		String upper = upperBound == Integer.MAX_VALUE ? "" : String.valueOf(upperBound);
		if (lower.isBlank() && upper.isBlank())
			return "";
		return lower + BuilderType.RANGE + upper;
	}
}