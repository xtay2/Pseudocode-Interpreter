package importing.filedata;

import java.util.Comparator;

import importing.filedata.paths.FilePath;

public record CallInfo(FilePath targetFile, String targetName, int paramCount) {

	public static Comparator<? super CallInfo> compareByFile() {
		return new Comparator<CallInfo>() {

			@Override
			public int compare(CallInfo o1, CallInfo o2) {
				return o1.targetFile.getAbsPath().compareTo(o2.targetFile.getAbsPath());
			}
		};
	}

}