package importing.filedata.paths;

import java.util.*;

import launching.*;

/**
 * Possible startlocation of a {@link FilePath}.
 */
public enum Location {
	
	SRC("src"), STD_LIB("stdlib"), USR_LIB("usrlib");
	
	private final String txt;
	
	private Location(String txt) {
		this.txt = txt;
	}
	
	/** Returns the absolute path of this {@link Location}. */
	String getAbsPath() {
		return switch (this) {
			case SRC -> Main.launchPath;
			case STD_LIB -> Main.libPath + "/stdlib";
			case USR_LIB -> Main.libPath + "/usrlib";
		};
	}
	
	/** Translates the prefix of an import into a {@link Location}-Object. */
	static Location fromString(String input) {
		for (Location l : values()) {
			if (l.toString().equals(input))
				return l;
		}
		throw new NoSuchElementException("There is no Location called" + input);
	}
	
	@Override
	public String toString() {
		return txt;
	}
	
}
