package misc.helper;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;

public class CollectionHelper {

	private CollectionHelper() {
		// Dead constructor
	}

	/** Merges two arrays of the same type. */
	public static <T> T[] merge(T[] arr1, T[] arr2) {
		T[] merged = Arrays.copyOf(arr1, arr1.length + arr2.length);
		System.arraycopy(arr2, 0, merged, arr1.length, arr2.length);
		return merged;
	}

	public static int[] merge(int[] arr1, int[] arr2) {
		int[] merged = Arrays.copyOf(arr1, arr1.length + arr2.length);
		System.arraycopy(arr2, 0, merged, arr1.length, arr2.length);
		return merged;
	}

	/**
	 * Finds an element in a {@link Collection} thats identifieable by a predicate.
	 * 
	 * @param <E> is the generic type of the content of the {@link Collection}.
	 * @param c is the {@link Collection} itself.
	 * @param p is the predicate that identifies the element.
	 * @return the element or null if none was found.
	 * @throws IllegalStateException if multiple elements matched the predicate.
	 */
	public static <E> E find(Collection<E> c, Predicate<? super E> p) throws IllegalStateException {
		List<E> results = c.stream().filter(p).toList();
		if (results.size() == 1)
			return results.get(0);
		if (results.size() > 1)
			throw new IllegalStateException("There are multiple elements in the collection that match the given predicate.\n"
					+ c.getClass().getSimpleName() + ": " + c + "\nMatches: " + results);
		return null;
	}
}
