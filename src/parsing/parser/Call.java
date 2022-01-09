package parsing.parser;

/** Identifies a call. Used exclusivly in the {@link Disassembler}. */
public class Call {

	final String name;

	final int arguments;

	public Call(String name, int arguments) {
		this.name = name;
		this.arguments = arguments;
	}

	@Override
	public String toString() {
		return name + "(" + arguments + ")";
	}
}
