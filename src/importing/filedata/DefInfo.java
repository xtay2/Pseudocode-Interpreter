package importing.filedata;

import importing.filedata.paths.FilePath;

public record DefInfo(FilePath defFile, String defName, int paramCount, int startLine, int endLine) {

	public boolean matches(CallInfo ci) {
		return paramCount == ci.paramCount() && defName.equals(ci.targetName()) && defFile.equals(ci.targetFile());
	}

}
