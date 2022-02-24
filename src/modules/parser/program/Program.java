package modules.parser.program;

import java.util.ArrayList;
import java.util.Iterator;

import modules.interpreter.Interpreter;

public final class Program implements Iterable<ProgramLine> {

	/** All lines of code, strongly indexed. */
	private final ArrayList<ProgramLine> program = new ArrayList<>(20);

	private boolean constructed = false;

	/**
	 * Adds/Replaces a specified, stripped line of code in this datastructure.
	 *
	 * @param content is the code in this line
	 * @param line    is the original line-index
	 * @return {@code true} if the line was changed.
	 */
	public void appendLine(String content, int line) {
		if (constructed)
			throw new AssertionError("This program already got constructed.");
		if (content == null)
			throw new NullPointerException("Line can be empty, but not null.");
		program.add(new ProgramLine(content, program.size(), line)); // Line was added.
	}

	/**
	 * Constructs the whole program and locks writing access.
	 */
	public void constructAndMerge() {
		constructed = true;
		for (ProgramLine line : program)
			line.construct();
		for (ProgramLine line : program)
			line.merge();
	}

	/**
	 * Return a ProgramLine
	 *
	 * @param i is the line-index
	 * @return the ProgramLine at that index.
	 */
	public ProgramLine getLine(int i) {
		try {
			return program.get(i);
		} catch (IndexOutOfBoundsException e) {
			throw new AssertionError("Tried to read line " + i + " from PROGRAM in Main.");
		}
	}

	@Override
	public String toString() {
		String out = "";
		for (ProgramLine l : program)
			out += l.toString() + "\n";
		return out;
	}

	/** Gets used in the {@link Interpreter}. */
	@Override
	public Iterator<ProgramLine> iterator() {
		return program.iterator();
	}
}
