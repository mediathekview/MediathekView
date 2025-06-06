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
import java.util.ArrayList;
import java.util.List;

public class MacFileUtils {

    private static final int FSREF_SIZE = 80;
    private static final int kFSPathMakeRefDoNotFollowLeafSymlink = 0x01;


    public static void moveToTrash(File... files) throws IOException {
        List<String> failed = new ArrayList<>();

        try (var arena = Arena.ofConfined()) {
            Linker linker = Linker.nativeLinker();
            //Carbon framework needs to be explicitely loaded...
            SymbolLookup cfLookup = SymbolLookup.libraryLookup("/System/Library/Frameworks/Carbon.framework/Carbon", Arena.global());

            var msfsp1 = cfLookup.find("FSPathMakeRefWithOptions").orElseThrow(() -> new RuntimeException("FSPathMakeRefWithOptions not found"));
            var FSPathMakeRefWithOptions = linker.downcallHandle(msfsp1,
                    FunctionDescriptor.of(ValueLayout.JAVA_INT, // return int
                            ValueLayout.ADDRESS, // const char* source
                            ValueLayout.JAVA_INT, // int options
                            ValueLayout.ADDRESS, // FSRef* fsref
                            ValueLayout.ADDRESS) // Byte* isDirectory (nullable)
            );

            var FSMoveObjectToTrashSync = linker.downcallHandle(
                    cfLookup.find("FSMoveObjectToTrashSync").orElseThrow(() -> new RuntimeException("FSMoveObjectToTrashSync not found")),
                    FunctionDescriptor.of(ValueLayout.JAVA_INT, // return int
                            ValueLayout.ADDRESS, // FSRef* source
                            ValueLayout.ADDRESS, // FSRef* target (nullable)
                            ValueLayout.JAVA_INT) // int options
            );

            for (File src : files) {
                /*if (!src.exists())
                    continue;*/
                var fsref = arena.allocate(FSREF_SIZE);
                var path = arena.allocateFrom(src.getAbsolutePath());

                int status = (int) FSPathMakeRefWithOptions.invoke(path, kFSPathMakeRefDoNotFollowLeafSymlink,
                        fsref, MemorySegment.NULL);
                if (status != 0) {
                    failed.add(src + " (FSRefMakeRefWithOptions: " + status + ")");
                    continue;
                }

                status = (int) FSMoveObjectToTrashSync.invoke(fsref, MemorySegment.NULL, 0);
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
