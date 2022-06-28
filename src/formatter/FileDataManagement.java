package formatter;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.Map.*;
import java.util.zip.*;

import importing.filedata.paths.*;
import launching.*;
import misc.supporting.*;

public interface FileDataManagement {
	
	String FILE_DATA_PATH = Main.launchPath + "/fileData.csv";
	
	/**
	 * This method updates the fileData.csv and returns a {@link List} of the {@link Path}s of all
	 * unformatted files.
	 */
	static List<FilePath> getUnformattedFiles(boolean forceFormat) {
		Output.print("Searching for unformatted files...");
		Map<FilePath, Long> oldFiles = readFileDataCSV();
		Map<FilePath, Long> newFiles = new HashMap<>();
		
		try {
			Files.walk(Path.of(Main.launchPath)).forEach(path -> {
				File f = path.toFile();
				if (f.isFile() && path.toString().endsWith(importing.filedata.File.EXTENSION)) {
					try(BufferedReader br = new BufferedReader(new FileReader(f))) {
						Checksum checksum = new Adler32();
						int c;
						while ((c = br.read()) != -1) {
							checksum.update(c);
						}
						newFiles.put(new FilePath(path), checksum.getValue());
					} catch (IOException e) { // Thrown by br.read();
						newFiles.put(new FilePath(path), null);
					}
				}
			});
		} catch (IOException e) {
			throw new AssertionError("Launch-Path should be checked by now.", e);
		}
		
		writeFileDataCSV(newFiles);
		return forceFormat ? newFiles.keySet().stream().toList() : filterUnformatted(oldFiles, newFiles);
	}
	
	/**
	 * Filters all non-empty new files that are not already formatted.
	 *
	 * @param oldFiles are the files which previously were inside the fileData.csv
	 * @param newFiles are all newely hashed files.
	 */
	private static List<FilePath> filterUnformatted(Map<FilePath, Long> oldFiles, Map<FilePath, Long> newFiles) {
		List<FilePath> unformatted = new ArrayList<>(newFiles.size());
		for (Entry<FilePath, Long> newFile : newFiles.entrySet()) {
			if (newFile.getValue() != 1) {
				long oldChecksum = oldFiles.getOrDefault(newFile.getKey(), 1l);
				if (oldChecksum == 1 || !newFile.getValue().equals(oldChecksum)) {
					unformatted.add(newFile.getKey());
				}
			}
		}
		Output.printAll("Unformatted Files", unformatted);
		return unformatted;
	}
	
	/**
	 * This method reads the fileData.csv and returns its entries in a {@link Map}. If the file is
	 * corrupted or doesn't exist, an empty map is returned.
	 */
	private static Map<FilePath, Long> readFileDataCSV() {
		Map<FilePath, Long> existingFiles = new HashMap<>();
		File f = new File(FILE_DATA_PATH);
		try(BufferedReader br = new BufferedReader(new FileReader(f))) {
			String line;
			while ((line = br.readLine()) != null) {
				String[] data = line.split(",");
				existingFiles.put(new FilePath(data[0]), Long.valueOf(data[1]));
			}
		} catch (Exception e) {
			return new HashMap<>();
		}
		return existingFiles;
	}
	
	/** Saves all fileData into a file at {@link #FILE_DATA_PATH}. The file gets completely replaced. */
	private static void writeFileDataCSV(Map<FilePath, Long> data) {
		List<String> lines = new ArrayList<>();
		for (Entry<FilePath, Long> e : data.entrySet()) {
			lines.add(e.getKey() + "," + e.getValue());
		}
		FileManager.writeFile(lines, Path.of(FILE_DATA_PATH));
	}
}
