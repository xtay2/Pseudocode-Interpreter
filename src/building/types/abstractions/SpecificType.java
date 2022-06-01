package building.types.abstractions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import building.expressions.normal.BuilderExpression;
import building.expressions.normal.brackets.BracketedExpression;
import building.expressions.normal.containers.ArrayAccess;

/**
 * Interface for all enums that hold raw types.
 */
public non-sealed interface SpecificType extends AbstractType {

	/**
	 * A specific type that has no followups. Examples for merged expressions are the
	 * {@link ArrayAccess} and the {@link BracketedExpression}.
	 */
	public static final SpecificType MERGED = new SpecificType() {

		private static final UnsupportedOperationException uoe = //
				new UnsupportedOperationException("No operations should get performed on a merged type.");

		@Override
		public AbstractType[] abstractExpected() {
			throw uoe;
		}

		@Override
		public BuilderExpression create(String arg, int lineID) {
			throw uoe;
		}
	};

	/**
	 * Nearly every {@link SpecificType} has expected strucures that can follow behind it. This method
	 * supports also {@link UnspecificType}s and gets called by {@link #expected()}.
	 *
	 * @return a non-null array.
	 */
	AbstractType[] abstractExpected();

	/**
	 * Nearly every {@link SpecificType} has expected strucures that can follow behind it. This method
	 * only returns the absolute subvalues.
	 *
	 * @return a non-null array.
	 */
	default SpecificType[] expected() {
		List<SpecificType> acc = new ArrayList<>();
		for (AbstractType t : abstractExpected()) {
			if (t instanceof UnspecificType us)
				acc.addAll(Arrays.asList(us.subValues()));
			else if (t instanceof SpecificType st)
				acc.add(st);
		}
		return Arrays.copyOf(acc.toArray(), acc.size(), SpecificType[].class);
	}

	@Override
	default boolean is(AbstractType other) {
		if (other instanceof UnspecificType ut)
			return this == other || Arrays.stream(ut.directSubValues()).anyMatch(st -> is(st));
		return this == other;
	}

	default BuilderExpression create(String arg, int lineID) {
		if (arg.equals(toString()))
			return new BuilderExpression(lineID, this);
		throw new AssertionError("Tried to create a BuilderValue out of an illegal String. " + arg + " -> " + this);
	}

	// Static methods

	/**
	 * Checks if the {@link String} matches a raw-type of the passed {@link SpecificType}.
	 *
	 * @param arg is the passed {@link String}.
	 * @param typeClass is the class of the {@link SpecificType}.
	 */
	static <T extends SpecificType> boolean equalsString(String arg, Class<T> typeClass) {
		return Arrays.stream(values(typeClass)).anyMatch(v -> v.toString().equals(arg));
	}

	/**
	 * Returns the first raw-type of the passed {@link SpecificType} that matches the passed
	 * {@link String}.
	 *
	 * @param <T> is the class-type of the return-value.
	 * @param arg is the passed {@link String}.
	 * @param typeClass is the class of the {@link SpecificType}.
	 */
	static <T extends SpecificType> T fromString(String arg, Class<T> typeClass) {
		return Arrays.stream(values(typeClass)).filter(v -> v.toString().equals(arg)).findFirst().get();
	}

	/**
	 * Calls the hidden, static method {@link Enum#values()} for any subclass of {@link SpecificType}.
	 *
	 * @param <T> is the class-type of the return-value.
	 * @param typeClass is the class of the {@link SpecificType}.
	 * @return an array of raw-types
	 */
	@SuppressWarnings("unchecked")
	private static <T extends SpecificType> T[] values(Class<T> typeClass) {
		try {
			return (T[]) typeClass.getMethod("values").invoke(null);
		} catch (Exception e) {
			e.printStackTrace();
			throw new AssertionError("Found no method called values in " + typeClass.getSimpleName());
		}
	}
}