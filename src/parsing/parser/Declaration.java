package parsing.parser;

import java.util.List;

/**
 * Identifies a function declaration. Used exclusivly in the
 * {@link Disassembler}.
 */
public class Declaration {

	final List<Call> calls;

	boolean getsCalled = false;

	final String name;

	final int start, end, arguments;

	public Declaration(String name, int start, int end, int arguments, List<Call> calls) {
		this.name = name;
		this.start = start;
		this.end = end;
		this.arguments = arguments;
		this.calls = calls;
	}

	@Override
	public String toString() {
		return name + arguments + " " + calls;
	}
}
