/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
        } catch (_: Exception) {
            //if there is any sort of error, pretend we are already running...
            close()
            return true
        }
    }

    private fun closeLock() {
        try {
            lock?.release()
            channel?.close()
        } catch (_: Exception) {
        }
    }

    private fun closeFile() {
        try {
            raf.close()
        }
        catch (_: Exception) {}
    }

    override fun close() {
        closeLock()
        closeFile()
    }
}