package mediathek.tool

import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager

/**
 * Helper class to possibly truncate path/filename combinations to the limitis of the used Operating Systems.
 */
class FileSpecifier(var path: String, var fileName: String) {
    /**
     * Check if the path/name combination is within the limits of the used OS.
     * If one exceeds limits it will truncate the filename.
     */
    fun checkLength() {
        if (SystemUtils.IS_OS_WINDOWS) {
            // in Win dürfen die Pfade nicht länger als 260 Zeichen haben (für die Infodatei kommen noch ".txt" dazu)
            if (path.length + 10 > WIN_MAX_PATH_LENGTH) {
                // es sollen für den Dateinamen mind. 10 Zeichen bleiben
                logger.error("Path too long: {}", path)
                path = GuiFunktionen.getHomePath()
            }
            if (path.length + fileName.length > WIN_MAX_PATH_LENGTH) {
                logger.error("FileSpecifier too long: {}", path)
                val maxNameL = WIN_MAX_PATH_LENGTH - path.length
                fileName = GuiFunktionen.cutName(fileName, maxNameL)
            }
        } else if (fileName.length > X_MAX_NAME_LENGTH) {
            logger.error("Name too long: {}", fileName)
            fileName = GuiFunktionen.cutName(fileName, X_MAX_NAME_LENGTH)
        }
    }

    companion object {
        private const val WIN_MAX_PATH_LENGTH = 250
        private const val X_MAX_NAME_LENGTH = 255
        private val logger = LogManager.getLogger()
    }
}