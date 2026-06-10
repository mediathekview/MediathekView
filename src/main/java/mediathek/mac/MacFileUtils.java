/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.mac;

import java.io.File;
import java.io.IOException;
import java.lang.foreign.Arena;
import java.lang.foreign.FunctionDescriptor;
import java.lang.foreign.Linker;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.SymbolLookup;
import java.lang.foreign.ValueLayout;
import java.util.ArrayList;
import java.util.List;

public class MacFileUtils {

    private static final int FSREF_SIZE = 80;
    private static final int NO_FOLLOW_LEAF_SYMLINK = 0x01;

	
    private MacFileUtils() {
        /* This utility class should not be instantiated */
    }

    /// Moves a file to the Finder trash.
    /// **Uses the macOS Carbon framework.**
    /// @param files List of files to be deleted.
    public static void moveToTrash(File... files) throws IOException {
        final List<String> failed = new ArrayList<>();

        try (var arena = Arena.ofConfined()) {
            final Linker linker = Linker.nativeLinker();
            //Carbon framework needs to be explicitely loaded...
            final SymbolLookup cfLookup = SymbolLookup.libraryLookup("/System/Library/Frameworks/Carbon.framework/Carbon", Arena.global());

            final var msfsp1 = cfLookup.find("FSPathMakeRefWithOptions").orElseThrow(() -> new RuntimeException("FSPathMakeRefWithOptions not found"));
            final var fsPathMakeRefWithOptions = linker.downcallHandle(msfsp1,
                    FunctionDescriptor.of(ValueLayout.JAVA_INT, // return int
                            ValueLayout.ADDRESS, // const char* source
                            ValueLayout.JAVA_INT, // int options
                            ValueLayout.ADDRESS, // FSRef* fsref
                            ValueLayout.ADDRESS) // Byte* isDirectory (nullable)
            );

            final var fsMoveObjectToTrashSync = linker.downcallHandle(
                    cfLookup.find("FSMoveObjectToTrashSync").orElseThrow(() -> new RuntimeException("FSMoveObjectToTrashSync not found")),
                    FunctionDescriptor.of(ValueLayout.JAVA_INT, // return int
                            ValueLayout.ADDRESS, // FSRef* source
                            ValueLayout.ADDRESS, // FSRef* target (nullable)
                            ValueLayout.JAVA_INT) // int options
            );

            for (File src : files) {
                /*if (!src.exists())
                    continue;*/
                final var fsref = arena.allocate(FSREF_SIZE);
                final var path = arena.allocateFrom(src.getAbsolutePath());

                int status = (int) fsPathMakeRefWithOptions.invoke(path, NO_FOLLOW_LEAF_SYMLINK,
                        fsref, MemorySegment.NULL);
                if (status != 0) {
                    failed.add(src + " (FSRefMakeRefWithOptions: " + status + ")");
                    continue;
                }

                status = (int) fsMoveObjectToTrashSync.invoke(fsref, MemorySegment.NULL, 0);
                if (status != 0) {
                    failed.add(src + " (FSMoveObjectToTrashSync: " + status + ")");
                }
            }
        }
        catch (Throwable t) {
            throw new IOException("Error while calling native functions", t);
        }

        if (!failed.isEmpty()) {
            throw new IOException("The following files could not be trashed: " + failed);
        }
    }
}
