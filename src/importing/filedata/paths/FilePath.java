package importing.filedata.paths;

import static formatter.basic.Formatter.WR;

import java.util.Objects;

import importing.filedata.File;
import misc.supporting.FileManager;

public class FilePath {

	protected final String filepath;

	protected final Location location;

	/** Clone-Constructor. */
	protected FilePath(FilePath path) {
		filepath = path.filepath;
		location = path.location;
	}

	public FilePath(String path) {
		if (!path.matches("(" + WR + "+\\.)+\\.?(" + WR + "+\\.)*" + WR + "+"))
			throw new Error("Invalid Path: \"" + path + "\"");
		int idxOfPrefix = path.indexOf('.');
		String prefix = path.substring(0, idxOfPrefix);
		path = path.substring(idxOfPrefix);
		location = Location.fromString(prefix);
		int idxOfSearch = path.indexOf("..");
		if (idxOfSearch != -1) {
			String startpoint = location.getAbsPath() + path.substring(0, idxOfSearch);
			String target = path.substring(path.lastIndexOf('.') + 1);
			path = FileManager.findPath(startpoint, target + File.EXTENSION) + target;
		}
		filepath = path;
	}

	/**
	 * Returns the absolute path on this machine to the matching .pc-File.
	 */
	public String getAbsPath() {
		return location.getAbsPath() + filepath.replaceAll("\\.", "/") + File.EXTENSION;
	}

	@Override
	public boolean equals(Object obj) {
		return obj instanceof FilePath fp && location == fp.location && filepath.equals(fp.filepath);
	}

	@Override
	public int hashCode() {
		return Objects.hash(filepath, location);
	}

	@Override
	public String toString() {
		return location + filepath;
	}
}