package importing.filedata.paths;

import building.expressions.abstractions.*;

/**
 * Immutable ID-Object that gets used to identify every {@link Expression} in an error-case.
 *
 * @see BlueprintPath
 */
public class DataPath extends FilePath {
	
	public final int orgLine;
	
	public DataPath(DataPath path) {
		super(path);
		this.orgLine = path.orgLine;
	}
	
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
		return super.toString() + "(" + orgLine + ")";
	}
	
}
