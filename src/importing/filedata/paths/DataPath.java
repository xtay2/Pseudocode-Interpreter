package importing.filedata.paths;

import building.expressions.abstractions.Expression;

/**
 * Immutable ID-Object that gets used to identify every {@link Expression} in an error-case.
 */
public class DataPath extends FilePath {

	public final int orgLine;

	public DataPath(FilePath path, int orgLine) {
		super(path);
		this.orgLine = orgLine;
	}

	@Override
	public boolean equals(Object obj) {
		return super.equals(obj) && orgLine == ((DataPath) obj).orgLine;
	}

	@Override
	public String toString() {
		return super.toString() + "(Line: " + orgLine + ")";
	}

}
