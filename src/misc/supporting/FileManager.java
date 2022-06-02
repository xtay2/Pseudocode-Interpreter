package misc.supporting;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import importing.filedata.paths.FilePath;
import misc.helper.StringHelper;

public final class FileManager {

	/**
	 * Write a List of lines into the textfile.
	 *
	 * @param content is the string.
	 * @param path is the relative path, where the file is stored.
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
	 * @param path is the relative path, where the file is stored.
	 */
	public static void writeFile(String content, Path path) {
		BufferedWriter writer;
		try {
			writer = new BufferedWriter(new FileWriter(path.toString(), StandardCharsets.UTF_8));
			writer.write(content);
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Finds a {@link File} in a specified directory.
	 *
	 * @param dir has to be the parent directory.
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
	 * Tries to find a File, when a bit of the path is missing.
	 *
	 * <pre>
	 * subfolder: "C:\Users\Pseudocode\MyProject"
	 * target: "Main.pc"
	 * returns: "package\subPackage"
	 * full path: "C:\Users\Pseudocode\MyProject\package\subPackage\Main.pc"
	 * </pre>
	 *
	 * @param subfolder is the starting point of the search. This has to be an absolute Path.
	 * @param target is the name of the target File or Folder.
	 * @return the path between subfolder and target
	 * @throws IOException when multiple, or no {@link File}s were found.
	 */
	public static String findPath(String subfolder, String target) throws IOException {
		List<Path> paths;
		try {
			paths = Files.walk(Path.of(subfolder)).filter(e -> e.endsWith(target)).toList();
		} catch (IOException e) {
			throw new FileNotFoundException("Couldn't find file \"" + subfolder + ".." + target + "\"");
		}
		if (paths.size() == 1) {
			String path = paths.get(0).toString().replace('\\', '/');
			path = path.substring(subfolder.length(), path.lastIndexOf(target));
			return path.replace('/', '.');
		}
		if (paths.isEmpty())
			throw new FileNotFoundException("Couldn't find any file at the search-path: \n\"" + subfolder + ".." + target + "\"");
		throw new IOException("There are multiple matches for the search-path: \n\"" + subfolder + ".." + target + "\"" //
				+ "\nMatches:" + StringHelper.enumerate(paths));
	}

	public static List<String> readFile(FilePath path) throws IOException {
		return Files.readAllLines(Paths.get(path.getAbsPath()));
	}
}
