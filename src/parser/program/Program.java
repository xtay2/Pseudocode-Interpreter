package parser.program;

import java.util.ArrayList;

public class Program {

	/** All lines of code, strongly indexed. */
	private ArrayList<ProgramLine> program = new ArrayList<>(20);

	/**
	 * Return a specified, stripped line of code as a string.
	 *
	 * @param i is the line-index
	 * @return a line of code as a string.
	 */
	public String readLine(int i) {
		return program.get(i).line;
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
	 * Adds/Replaces a specified, stripped line of code in this datastructure.
	 *
	 * @param i       is the line-index
	 * @param content is the code in this line
	 * @return {@code true} if the line was changed.
	 */
	public boolean writeLine(int index, String content) {
		if (content == null)
			throw new NullPointerException("Line can be empty, but not null.");
		if (index < program.size()) {
			if(readLine(index).equals(content)) //Line hasn't changed.
				return false;
			program.set(index, new ProgramLine(content, index, this)); //Line was changed.
		} else
			program.add(index, new ProgramLine(content, index, this)); //Line was added.
		return true;
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
