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
import java.lang.foreign.*;
import java.lang.invoke.MethodHandle;
import java.util.ArrayList;
import java.util.List;

public class MacFileUtils {

    private static final SymbolLookup symbols = SymbolLookup.loaderLookup();
    private static final int FSREF_SIZE = 80;
    private static final int kFSPathMakeRefDoNotFollowLeafSymlink = 0x01;
    private static final MethodHandle FSPathMakeRefWithOptions;
    private static final MethodHandle FSMoveObjectToTrashSync;

    static {
        Linker linker = Linker.nativeLinker();
        FSPathMakeRefWithOptions = linker.downcallHandle(
                symbols.find("FSPathMakeRefWithOptions").orElseThrow(),
                FunctionDescriptor.of(ValueLayout.JAVA_INT, // return int
                        ValueLayout.ADDRESS, // const char* source
                        ValueLayout.JAVA_INT, // int options
                        ValueLayout.ADDRESS, // FSRef* fsref
                        ValueLayout.ADDRESS) // Byte* isDirectory (nullable)
        );

        FSMoveObjectToTrashSync = linker.downcallHandle(
                symbols.find("FSMoveObjectToTrashSync").orElseThrow(),
                FunctionDescriptor.of(ValueLayout.JAVA_INT, // return int
                        ValueLayout.ADDRESS, // FSRef* source
                        ValueLayout.ADDRESS, // FSRef* target (nullable)
                        ValueLayout.JAVA_INT) // int options
        );

    }

    public static void moveToTrash(File... files) throws IOException {
        List<String> failed = new ArrayList<>();

        try (var arena = Arena.ofConfined()) {
            for (File src : files) {
                var fsref = arena.allocate(FSREF_SIZE);
                var path = arena.allocateFrom(src.getAbsolutePath());

                int status = (int) FSPathMakeRefWithOptions.invoke(
                        path,
                        kFSPathMakeRefDoNotFollowLeafSymlink,
                        fsref,
                        MemorySegment.NULL
                );

                if (status != 0) {
                    failed.add(src + " (FSRefMakeRefWithOptions: " + status + ")");
                    continue;
                }

                status = (int) FSMoveObjectToTrashSync.invoke(
                        fsref,
                        MemorySegment.NULL,
                        0
                );

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
