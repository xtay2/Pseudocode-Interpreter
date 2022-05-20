package importing.filedata.paths;

public class FilePath implements Comparable<FilePath> {

	private final String filepath;

	private final Location location;

	public FilePath(String path) {
		// TODO Implement me!
		filepath = "";
		location = null;
	}

	/**
	 * Returns the absolute path on this machine to the matching .pc-File.
	 */
	public String getAbsPath() {
		return location.getAbsPath();
	}

	@Override
	public boolean equals(Object obj) {
		return obj instanceof FilePath fp && location == fp.location && filepath.equals(fp.filepath);
	}

	@Override
	public int compareTo(FilePath fp) {
		return getAbsPath().compareTo(fp.getAbsPath());
	}

}