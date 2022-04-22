package misc.tests;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import building.types.abstractions.AbstractType;
import building.types.abstractions.SpecificType;
import building.types.abstractions.SuperType;
import building.types.abstractions.UnspecificType;

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
