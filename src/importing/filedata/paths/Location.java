package importing.filedata.paths;

import launching.Main;

/**
 * Possible startlocation of a {@link FilePath}.
 */
public enum Location {

	SRC, STD_LIB, USR_LIB;

	/**
	 * Returns the absolute path of this {@link Location}.
	 */
	String getAbsPath() {
		return switch (this) {
			case SRC -> Main.launchPath;
			case STD_LIB -> Main.libPath + "stdlib";
			case USR_LIB -> Main.libPath + "usrlib";
		};
	}

}
