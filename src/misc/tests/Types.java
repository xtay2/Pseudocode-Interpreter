package misc.tests;

import java.util.*;
import java.util.stream.*;

import org.junit.jupiter.api.*;

import building.types.abstractions.*;

public class Types {
	
	@Test
	void test() {
		AbstractType[] everyType = SuperType.values();
		for (AbstractType t : everyType) {
			try {
				hasDuplicateExpected(t);
			} catch (UnsupportedOperationException e) {
				System.out.println(e);
			}
		}
	}
	
	/** Checks, if a type stores multiple identical {@link SpecificType}s. */
	private void hasDuplicateExpected(AbstractType t) {
		List<SpecificType> exp = new ArrayList<>();
		if (t instanceof UnspecificType ut)
			Arrays.stream(ut.subValues()).forEach(e -> hasDuplicateExpected(e));
		else if (t instanceof SpecificType st) {
			exp.addAll(Arrays.asList(st.expected()));
			if (exp.size() > new HashSet<>(exp).size()) {
				System.out.println(findDuplicates(Arrays.asList(st.expected())) + " in " + t);
				throw new AssertionError("Duplicate expected in " + t);
			}
			assert !exp.contains(null) : "Expected types for " + t + " contain null.";
		}
	}
	
	private <T> Set<T> findDuplicates(Collection<T> collection) {
		Set<T> uniques = new HashSet<>();
		return collection.stream().filter(e -> !uniques.add(e)).collect(Collectors.toSet());
	}
}
