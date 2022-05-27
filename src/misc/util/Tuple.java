package misc.util;

import java.util.Objects;

/**
 * Immutable Tupel
 *
 * @param <V1> type of the first entry
 * @param <V2> type of the second entry
 */
public class Tuple<V1, V2> {

	public final V1 val1;
	public final V2 val2;

	/**
	 * Creates a new, immutable {@link Tuple}.
	 *
	 * @param val1 is the first value.
	 * @param val2 is the second value.
	 */
	public Tuple(V1 val1, V2 val2) {
		this.val1 = val1;
		this.val2 = val2;
	}

	@Override
	public boolean equals(Object obj) {
		return obj instanceof Tuple<?, ?> t && val1.equals(t.val1) && val2.equals(t.val2);
	}

	@Override
	public int hashCode() {
		return Objects.hash(val1, val2);
	}

	@Override
	public String toString() {
		return "(" + val1 + ", " + val2 + ")";
	}
}
