package exceptions;

import building.expressions.abstractions.Expression;
import importing.filedata.paths.DataPath;
import importing.filedata.paths.FilePath;

@SuppressWarnings("serial")
public abstract class PseudocodeException extends Error {

	public PseudocodeException(DataPath dataPath, String msg) {
		super("at " + dataPath);
	}

	public PseudocodeException(Expression exp, String msg) {
		this(exp.getDataPath(), msg);
	}

	public static void main(String[] args) {
		throw new TestException(new DataPath(new FilePath("stdlib.System"), 20), "Something went wrong");
	}
}

class TestException extends PseudocodeException {

	public TestException(DataPath dataPath, String msg) {
		super(dataPath, msg);
	}

}