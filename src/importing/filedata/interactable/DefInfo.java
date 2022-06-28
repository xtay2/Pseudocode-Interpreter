package importing.filedata.interactable;

import building.expressions.main.functions.*;
import importing.filedata.paths.*;

/**
 * Saves all necessary importing-data for a {@link Definition}.
 *
 * @param defFile is the original {@link File}.
 * @param defName is the name of the definition.
 * @param paramCount is the amount of parameters.
 * @param startLine is the line-index in which the coresponding func keywords is written.
 * @param endLine is the line of the closing symbol or the startline if isNative == true.
 * @param isNative tells if this definition is native. (Has no outgoing calls, one-liner)
 */
public record DefInfo(FilePath defFile, String defName, int paramCount, int startLine, int endLine, boolean isNative) {
	
	/**
	 * Compares this to a {@link CallInfo}.
	 *
	 * @param ci is a {@link CallInfo} that maybe calls this def
	 * @return true if all params match this
	 */
	public boolean matches(CallInfo ci) {
		return paramCount == ci.paramCount() && defName.equals(ci.targetName()) && defFile.equals(ci.targetFile());
	}
	
	/**
	 * Compares this to a {@link CallInfo} which {@link CallInfo#targetFile()} isn't known.
	 *
	 * @param callName is the name of the called def
	 * @param params is the amount of params of the called def
	 * @return true if both params match this
	 */
	public boolean matches(String callName, int params) {
		return params == paramCount && defName.equals(callName);
	}
	
}
