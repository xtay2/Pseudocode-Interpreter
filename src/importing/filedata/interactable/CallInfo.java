package importing.filedata.interactable;

import java.util.*;

import importing.filedata.paths.*;

public record CallInfo(DataPath originPath, FilePath targetFile, String targetName, int paramCount) {
	
	@Override
	public boolean equals(Object obj) {
		return obj instanceof CallInfo ci //
				&& targetFile.equals(ci.targetFile) //
				&& targetName.equals(ci.targetName) //
				&& paramCount == ci.paramCount;
	}
	
	public static Comparator<? super CallInfo> compareByFile() {
		return new Comparator<CallInfo>() {
			
			@Override
			public int compare(CallInfo o1, CallInfo o2) {
				return o1.targetFile.getAbsPath().compareTo(o2.targetFile.getAbsPath());
			}
		};
	}
	
}