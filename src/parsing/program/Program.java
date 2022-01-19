package parsing.program;

import java.util.ArrayList;

public class Program {

	/** All lines of code, strongly indexed. */
	private ArrayList<ProgramLine> program = new ArrayList<>(20);

	/**
	 * Adds/Replaces a specified, stripped line of code in this datastructure.
	 *
	 * @param content is the code in this line
	 * @param line    is the original line-index
	 * @return {@code true} if the line was changed.
	 */
	public boolean appendLine(String content, int line) {
		if (content == null)
			throw new NullPointerException("Line can be empty, but not null.");
		ProgramLine newline = new ProgramLine(content, program.size(), line, this);
		program.add(newline); // Line was added.
		newline.construct();
		return true;
	}

	/**
	 * Return a ProgramLine
	 *
	 * @param i is the line-index
	 * @return the ProgramLine at that index.
	 */
	public ProgramLine getLine(int i) {
		return program.get(i);
	}

	/**
	 * Return a specified, stripped line of code as a string.
	 *
	 * @param i is the line-index
	 * @return a line of code as a string.
	 */
	public String readLine(int i) {
		return program.get(i).line;
	}

	public int size() {
		return program.size();
	}

	@Override
	public String toString() {
		String out = "";
		for (ProgramLine l : program)
			out += l.toString() + "\n";
		return out;
	}

}
