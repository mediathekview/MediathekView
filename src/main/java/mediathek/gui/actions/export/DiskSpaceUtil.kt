package mediathek.gui.actions.export

import org.apache.logging.log4j.LogManager
import java.io.File
import java.nio.file.Files

internal object DiskSpaceUtil {
    private val logger = LogManager.getLogger()

    /**
     * Check if there is more than 1 GB of free disk space available.
     */
    fun enoughDiskSpace(selectedFile: File): Boolean {
        try {
            var tempFileCreated = false

            if (!selectedFile.exists()) {
                selectedFile.createNewFile()
                tempFileCreated = true
            }
            val store = Files.getFileStore(selectedFile.toPath())
            val availFreeSpace = store.usableSpace / (1024 * 1024) //Megabyte

            if (tempFileCreated)
                selectedFile.delete()

            return availFreeSpace >= 1024
        }
        catch (e: Exception) {
            // in case of error pretend there is space available as it may fail on network drives
            logger.error("enoughDiskSpace failed!", e)
            return true
        }
    }
}