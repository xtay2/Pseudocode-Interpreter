package misc.helper;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

public final class FileManager {

	/**
	 * Count the number of lines in a textfile.
	 *
	 * @param path is the relative path, where the file is stored.
	 * @return the linecount as int
	 */
//	public static int countLinesInFile(String path) {
//		try (FileReader input = new FileReader(path); LineNumberReader count = new LineNumberReader(input);) {
//			count.skip(Long.MAX_VALUE);
//			return count.getLineNumber();
//		} catch (FileNotFoundException e) {
//			e.printStackTrace();
//		} catch (IOException e) {
//			e.printStackTrace();
//		}
//		return -1;
//	}

	/**
	 * Convert a textfile to an array of string.
	 *
	 * @param path is the relative path, where the file is stored.
	 * @return the content of the file, line by line, as a string array.
	 */
//	public static String[] fileToLineArray(String path) {
//		String[] programm = new String[countLinesInFile(path)];
//		try (BufferedReader br = new BufferedReader(new FileReader(path))) {
//			for (int i = 0; i < programm.length; i++)
//				programm[i] = br.readLine();
//		} catch (IOException e) {
//			e.printStackTrace();
//		}
//		return programm;
//	}

	/**
	 * Convert a textfile to a string.
	 *
	 * @param path is the relative path, where the file is stored.
	 * @return the content of the file as a string.
	 */
//	public static String readFile(String path) {
//		String out = "";
//		try (BufferedReader br = new BufferedReader(new FileReader(path))) {
//			out = br.readLine();
//		} catch (IOException e) {
//			e.printStackTrace();
//		}
//		return out;
//	}

	/**
	 * Write a List of lines into the textfile.
	 *
	 * @param content is the string.
	 * @param path    is the relative path, where the file is stored.
	 */
	public static void writeFile(List<String> content, Path path) {
		String res = "";
		for (String line : content)
			res += line + "\n";
		writeFile(res.stripTrailing(), path);
	}

	/**
	 * Write a string into the textfile.
	 *
	 * @param content is the string.
	 * @param path    is the relative path, where the file is stored.
	 */
	public static void writeFile(String content, Path path) {
		BufferedWriter writer;
		try {
			writer = new BufferedWriter(new FileWriter(path.toString()));
			writer.write(content);
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Finds a {@link File} in a specified directory.
	 * 
	 * @param dir    has to be the parent directory.
	 * @param target is the name of the quested {@link File}.
	 * @return the quested {@link File} or null if nothing was found.
	 */
	public static File findFileDir(File dir, String target) {
		File[] content = dir.listFiles();
		if (content == null)
			return null;
		for (File f : content) {
			if (f.isDirectory()) {
				File targetPath = findFileDir(f, target);
				if (targetPath != null)
					return targetPath;
			} else if (f.isFile() && f.getName().equals(target))
				return f;
		}
		return null;
	}

	/**
	 * Write a Array of lines into the textfile.
	 *
	 * @param content is the string.
	 * @param path    is the relative path, where the file is stored.
	 */
//	public static void writeFile(String[] content, String path) {
//		String res = "";
//		for (String line : content)
//			res += line + "\n";
//		writeFile(res.stripTrailing(), path);
//	}
}
