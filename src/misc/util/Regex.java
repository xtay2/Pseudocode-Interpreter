package misc.util;

import java.util.regex.*;

import building.expressions.normal.containers.name.*;
import misc.constants.*;

public enum Regex {
	
	// ---------------------------------------------------------------------------
	/** Lowercase Regex ASCII */
	ASCII_LC("[a-z]"),
	
	/** Uppercase Regex ASCII */
	ASCII_UC("[A-Z]"),
	
	/** ASCII Regex */
	ASCII("[a-zA-Z]"),
	
	// ---------------------------------------------------------------------------
	/** Greek lowercase Regex */
	GREEK_LC("[" + GreekSymbol.ALPHA.lower + "-" + GreekSymbol.OMEGA.lower + "]"),
	
	/** Greek uppercase Regex */
	GREEK_UC("[" + GreekSymbol.ALPHA.upper + "-" + GreekSymbol.OMEGA.upper + "]"),
	
	/** Greek characters Regex */
	GREEK("(" + GREEK_LC + "|" + GREEK_UC + ")"),
	
	// ---------------------------------------------------------------------------
	/** Lowercase Umlaute */
	UMLAUT_LC("[äöüß]"),
	
	/** Uppercase Umlaute */
	UMLAUT_UC("[ÄÖÜ]"),
	
	/** Umlaut Regex */
	UMLAUT("(" + UMLAUT_LC + "|" + UMLAUT_UC + ")"),
	
	// ---------------------------------------------------------------------------
	/** Alphabetical Lowercase */
	ALPHA_LC("(" + ASCII_LC + "|" + GREEK_LC + "|" + UMLAUT_LC + ")"),
	
	/** Alphabetical Uppercase */
	ALPHA_UC("(" + ASCII_UC + "|" + GREEK_UC + "|" + UMLAUT_UC + ")"),
	
	/** ASCII chars, Greek chars, umlaut */
	ALPHABETICAL("(" + ASCII + "|" + GREEK + "|" + UMLAUT + ")"),
	
	// ---------------------------------------------------------------------------
	/** Lowercase Alphabetical & digits. */
	ALPHANUM_LC("(" + ALPHA_LC + "|\\d)"),
	
	/** Uppercase Alphabetical & digits. */
	ALPHANUM_UC("(" + ALPHA_UC + "|\\d)"),
	
	/** Alphabetical & digits & underscores. */
	ALPHANUM("(" + ALPHA_LC + "|" + ALPHA_UC + "|\\d|_)"),
	
	// ---------------------------------------------------------------------------
	/**
	 * Alphanumerical Lowercase with atleast one alphabetic char.
	 *
	 * @see VarName
	 */
	WR_LC("(" + ALPHANUM_LC + "*(" + ALPHA_LC + ")+" + ALPHANUM_LC + "*)"),
	
	/**
	 * Alphanumerical Uppercase with atleast one alphabetic char.
	 *
	 * @see ConstName
	 */
	WR_UC("(" + ALPHA_UC + "*(" + ALPHA_UC + ")+" + ALPHA_UC + "*)"),
	
	/**
	 * Alphanumerical with underscores and atleast one alphabetic char.
	 *
	 * @see Name
	 */
	WR("(" + ALPHANUM + "*(" + ALPHABETICAL + ")+" + ALPHANUM + "*)"),
	
	// ---------------------------------------------------------------------------
	
	/**
	 * Blueprint-name. Starts with an {@link #ALPHA_UC} and contains only {@link #ALPHABETICAL}
	 * characters.
	 */
	BP(ALPHA_UC + "" + ALPHABETICAL + "+");
	
	public final Pattern pattern;
	
	private Regex(String string) {
		pattern = Pattern.compile(string);
	}
	
	/** Similar to {@link String#matches(String)} */
	public boolean matches(String s) {
		return pattern.matcher(s).matches();
	}
	
	@Override
	public String toString() {
		return pattern.pattern();
	}
}
