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

package mediathek.mac

import java.io.File
import java.io.IOException
import java.lang.foreign.*

object MacFileUtils {
    private const val FSREF_SIZE = 80L
    private const val K_FS_PATH_MAKE_REF_DO_NOT_FOLLOW_LEAF_SYMLINK = 0x01

    /**
     * Moves files to the Finder trash.
     *
     * Uses the macOS Carbon framework.
     */
    @Throws(IOException::class)
    @Suppress("LocalVariableName")
    fun moveToTrash(vararg files: File) {
        val failed = mutableListOf<String>()

        try {
            Arena.ofConfined().use { arena ->
                val linker = Linker.nativeLinker()
                val carbonLookup = SymbolLookup.libraryLookup(
                    "/System/Library/Frameworks/Carbon.framework/Carbon",
                    Arena.global(),
                )

                val FSPathMakeRefWithOptions = linker.downcallHandle(
                    carbonLookup.find("FSPathMakeRefWithOptions")
                        .orElseThrow { RuntimeException("FSPathMakeRefWithOptions not found") },
                    FunctionDescriptor.of(
                        ValueLayout.JAVA_INT,
                        ValueLayout.ADDRESS,
                        ValueLayout.JAVA_INT,
                        ValueLayout.ADDRESS,
                        ValueLayout.ADDRESS,
                    ),
                )

                val FSMoveObjectToTrashSync = linker.downcallHandle(
                    carbonLookup.find("FSMoveObjectToTrashSync")
                        .orElseThrow { RuntimeException("FSMoveObjectToTrashSync not found") },
                    FunctionDescriptor.of(
                        ValueLayout.JAVA_INT,
                        ValueLayout.ADDRESS,
                        ValueLayout.ADDRESS,
                        ValueLayout.JAVA_INT,
                    ),
                )

                for (src in files) {
                    val fsRef = arena.allocate(FSREF_SIZE)
                    val path = arena.allocateFrom(src.absolutePath)

                    var status = FSPathMakeRefWithOptions.invoke(
                        path,
                        K_FS_PATH_MAKE_REF_DO_NOT_FOLLOW_LEAF_SYMLINK,
                        fsRef,
                        MemorySegment.NULL,
                    ) as Int
                    if (status != 0) {
                        failed.add("$src (FSRefMakeRefWithOptions: $status)")
                        continue
                    }

                    status = FSMoveObjectToTrashSync.invoke(fsRef, MemorySegment.NULL, 0) as Int
                    if (status != 0) {
                        failed.add("$src (FSMoveObjectToTrashSync: $status)")
                    }
                }
            }
        } catch (throwable: Throwable) {
            throw IOException("Error while calling native functions", throwable)
        }

        if (failed.isNotEmpty()) {
            throw IOException("The following files could not be trashed: $failed")
        }
    }
}
