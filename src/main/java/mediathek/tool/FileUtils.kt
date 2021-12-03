package mediathek.tool;

import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class FileUtils {
    /**
     * Move a file to the OS trash if supported, otherwise delete it.
     * @param filePath the pathe to the file to be deleted.
     * @throws IOException any occuring exception.
     */
    public static void moveToTrash(@NotNull Path filePath) throws IOException {
        final var fileUtils = com.sun.jna.platform.FileUtils.getInstance();
        if (fileUtils.hasTrash())
            fileUtils.moveToTrash(new File[]{filePath.toFile()});
        else
            Files.deleteIfExists(filePath);
    }
}
