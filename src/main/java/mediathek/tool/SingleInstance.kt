package mediathek.tool

import mediathek.config.StandardLocations
import java.io.Closeable
import java.io.IOException
import java.io.RandomAccessFile
import java.nio.channels.FileChannel
import java.nio.channels.FileLock
import java.nio.file.Files

/**
 * Prevents startup of multiple instances
 */
class SingleInstance : Closeable {
    private var channel: FileChannel? = null
    private var lock: FileLock? = null
    private val raf: RandomAccessFile = RandomAccessFile(StandardLocations.getLockFilePath().toFile(), "rw")

    fun isAppAlreadyActive(): Boolean
    {
        try {
            channel = raf.channel
            lock = channel?.tryLock()
            if (lock == null) {
                //we could not acquire the lock because another app already holds it...we are already active
                closeLock()
                return true
            }

            //delete the lockfile when VM gets shut down
            Runtime.getRuntime().addShutdownHook(Thread {
                close()

                try {
                    Files.deleteIfExists(StandardLocations.getLockFilePath())
                } catch (ex: IOException) {
                    ex.printStackTrace()
                }
            })
            return false
        } catch (e: Exception) {
            //if there is any sort of error, pretend we are already running...
            close()
            return true
        }
    }

    private fun closeLock() {
        try {
            lock?.release()
            channel?.close()
        } catch (ignored: Exception) {
        }
    }

    private fun closeFile() {
        try {
            raf.close()
        }
        catch (ignored: Exception) {}
    }

    override fun close() {
        closeLock()
        closeFile()
    }
}