package interpreting.program;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.stream.Stream;

import importing.filedata.paths.DataPath;
import interpreting.modules.interpreter.Interpreter;

public final class Program implements Iterable<ProgramLine> {

	/** All lines of code, strongly indexed. */
	private final ArrayList<ProgramLine> program = new ArrayList<>(20);

	private boolean constructed = false;

	/**
	 * Adds/Replaces a specified, stripped line of code in this datastructure.
	 *
	 * @param content is the code in this line
	 * @param line is the original line-index
	 * @return {@code true} if the line was changed.
	 */
	public void appendLine(String content, DataPath dataPath) {
		if (constructed)
			throw new AssertionError("This program already got constructed.");
		if (content == null)
			throw new NullPointerException("Line can be empty, but not null.");
		program.add(new ProgramLine(content, program.size(), dataPath)); // Line was added.
	}

	/**
	 * Constructs the whole program and locks writing access.
	 */
	public void constructAndMerge() {
		constructed = true;
		// Construct BuilderExpressions
		for (ProgramLine line : program)
			line.construct();

		// Merge to Expressions
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
		constructedCheck();
		try {
			return program.get(i);
		} catch (IndexOutOfBoundsException e) {
			throw new AssertionError("Tried to read line " + i + " from PROGRAM in Main. Size: " + program.size());
		}
	}

	/** Finds a line of code through the {@link DataPath}. */
	public String find(DataPath location) {
		constructedCheck();
		try {
			return stream().filter(e -> e.dataPath.equals(location)).findFirst().get().line;
		} catch (NoSuchElementException e) {
			throw new AssertionError("A valid datapath should link to a valid file.\nPath was: " + location, e);
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
		constructedCheck();
		return program.iterator();
	}

	/** Returns the number of lines in this program. */
	public int size() {
		return program.size();
	}

	/**
	 * Returns a {@link Stream} of this program.
	 */
	public Stream<ProgramLine> stream() {
		constructedCheck();
		return program.stream();
	}

	public boolean isConstructed() {
		return constructed;
	}

	/**
	 * Throws an {@link AssertionError} if the program isn't {@link #constructed} yet. This should get
	 * added to all major read operations.
	 */
	private void constructedCheck() {
		assert constructed : "The program is not yet initialized and cannot be searched.";
	}
}
