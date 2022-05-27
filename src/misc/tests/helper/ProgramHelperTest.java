package misc.tests.helper;

import static misc.helper.ProgramHelper.getAllCallsInLine;

import java.util.Set;

import org.junit.Test;

public class ProgramHelperTest {

	@Test
	public void getAllCallsInLineTest() {
		// @formatter:off
		setAssert(Set.of("foo()"								), 	getAllCallsInLine("foo()"));
		setAssert(Set.of("foo()", "bar()"						), 	getAllCallsInLine("foo() + bar()"));
		setAssert(Set.of(										),	getAllCallsInLine("func foo()"));
		setAssert(Set.of("bar()"								), 	getAllCallsInLine("func foo() -> text: return bar();"));
		setAssert(Set.of("bar(buzz())", "buzz()"				), 	getAllCallsInLine("func foo() -> text: return bar(buzz());"));
		setAssert(Set.of("foo(bar(), buzz())", "bar()", "buzz()"), 	getAllCallsInLine("foo(bar(), buzz())"));
		// @formatter:on
	}

	private <T> void setAssert(Set<T> expected, Set<T> result) {
		assert expected.containsAll(result) : result + "L:(" + result.size() + ") instead of " + expected + "L:(" + expected.size() + ")";
	}

}
