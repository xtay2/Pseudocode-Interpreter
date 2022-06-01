package importing.filedata.paths;

import static formatter.basic.Formatter.WR;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.Objects;

import importing.filedata.File;
import launching.Main;
import misc.supporting.FileManager;

public class FilePath {

	protected final String filepath;

	protected final Location location;

	/** Clone-Constructor. */
	protected FilePath(FilePath path) {
		filepath = path.filepath;
		location = path.location;
	}

	/**
	 * Creates a new {@link FilePath}.
	 *
	 * @param path is the path as a {@link String}.
	 * @throws InvalidPathException if the string has the wrong format.
	 * @throws IOException if there are two ore no files with this path.
	 */
	public FilePath(String path) throws InvalidPathException, IOException {
		if (!path.matches("(" + WR + "+\\.)+\\.?(" + WR + "+\\.)*" + WR + "+"))
			throw new InvalidPathException(path, "The entered path has an invalid format");
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
		if (!Files.exists(Path.of(getAbsPath())))
			throw new FileNotFoundException("Couldn't find: " + this + "\n(" + getAbsPath() + ")");
	}

	/**
	 * Builds a {@link FilePath} from a trusted lib- or src-path.
	 */
	public FilePath(Path trustedPath) {
		String path = trustedPath.toString().replace('\\', '/');
		if (path.startsWith(Main.launchPath)) {
			path = path.substring(Main.launchPath.length());
			location = Location.SRC;
		} else if (trustedPath.startsWith(Main.libPath)) {
			path = path.substring(Main.libPath.length());
			location = Location.fromString(path.substring(0, 6));
		} else
			throw new InvalidPathException(path, "This path is neither in the src dir, nor any library");
		if (path.endsWith(File.EXTENSION))
			path = path.substring(0, path.length() - File.EXTENSION.length());
		filepath = path.replace('/', '.');
	}

	/**
	 * Returns the absolute path on this machine to the matching .pc-File.
	 */
	public String getAbsPath() {
		return location.getAbsPath() + filepath.replaceAll("\\.", "/") + File.EXTENSION;
	}

	/** Returns the name of the file without its extension. */
	public String getName() {
		return filepath.substring(filepath.lastIndexOf('.') + 1);
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