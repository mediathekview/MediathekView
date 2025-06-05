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

package mediathek.windows;

import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.lang.foreign.*;
import java.lang.invoke.MethodHandle;
import java.nio.charset.StandardCharsets;

public class WindowsFileUtils {

    private static final MethodHandle SHFileOperationW;

    private static final int FO_DELETE = 3;
    private static final int FOF_ALLOWUNDO = 0x40;
    private static final int FOF_NOCONFIRMATION = 0x10;
    private static final int FOF_NO_UI = FOF_NOCONFIRMATION;

    static final GroupLayout SHFILEOPSTRUCTW_LAYOUT = MemoryLayout.structLayout(
            ValueLayout.ADDRESS.withName("hwnd"),
            ValueLayout.JAVA_INT.withName("wFunc"),
            MemoryLayout.paddingLayout(4), // Ensure proper alignment
            ValueLayout.ADDRESS.withName("pFrom"),
            ValueLayout.ADDRESS.withName("pTo"),
            ValueLayout.JAVA_SHORT.withName("fFlags"),
            MemoryLayout.paddingLayout(2), // Align next 4-byte field
            ValueLayout.JAVA_INT.withName("fAnyOperationsAborted"),
            ValueLayout.ADDRESS.withName("hNameMappings"),
            ValueLayout.ADDRESS.withName("lpszProgressTitle")
    ).withName("SHFILEOPSTRUCTW");



    static {
        var linker = Linker.nativeLinker();
        SymbolLookup shell32 = SymbolLookup.libraryLookup("shell32", Arena.global());
        SHFileOperationW = linker.downcallHandle(
                shell32.find("SHFileOperationW").orElseThrow(),
                FunctionDescriptor.of(ValueLayout.JAVA_INT, MemoryLayout.structLayout(SHFILEOPSTRUCTW_LAYOUT))
        );
    }

    /**
     * Converts a list of files into a Windows multi-string segment:
     * UTF-16LE encoded, single null between strings, double null at the end.
     *
     * @param arena MemoryArena to allocate in (e.g. confined or scoped)
     * @param files Paths to include
     * @return MemorySegment containing double-null-terminated UTF-16LE multi-string
     */
    protected static MemorySegment toUtf16MultiStringSegment(Arena arena, File... files) {
        if (files == null || files.length == 0)
            throw new IllegalArgumentException("At least one path must be provided");

        var joined = new StringBuilder();
        for (var path : files) {
            joined.append(path.getAbsolutePath()).append('\0');
        }
        joined.append('\0'); // Final double-null

        var utf16 = joined.toString().getBytes(StandardCharsets.UTF_16LE);
        //debugPrintUtf16(utf16);
        var segment = arena.allocate(utf16.length, 2); // UTF-16 requires 2-byte alignment
        segment.copyFrom(MemorySegment.ofArray(utf16));
        return segment;
    }

    @SuppressWarnings("unused")
    protected static void debugPrintUtf16(byte[] utf16) {
        for (int i = 0; i < utf16.length; i += 2) {
            int ch = ((utf16[i+1] & 0xFF) << 8) | (utf16[i] & 0xFF);
            System.out.printf("%04x ", ch);
        }
        System.out.println();  // You should see: C: \ 0 \ 0 0 \ 0 (final two nulls)
    }

    public static void moveToTrash(@NotNull File... files) throws IOException {
        try (var arena = Arena.ofConfined()) {
            var pFrom = toUtf16MultiStringSegment(arena, files);
            var shfileop = arena.allocate(SHFILEOPSTRUCTW_LAYOUT);

            SHFILEOPSTRUCTW_LAYOUT.varHandle(MemoryLayout.PathElement.groupElement("wFunc")).set(shfileop, 0L, FO_DELETE);
            SHFILEOPSTRUCTW_LAYOUT.varHandle(MemoryLayout.PathElement.groupElement("pFrom")).set(shfileop, 0L, pFrom);
            SHFILEOPSTRUCTW_LAYOUT.varHandle(MemoryLayout.PathElement.groupElement("pTo")).set(shfileop, 0L, MemorySegment.NULL);
            SHFILEOPSTRUCTW_LAYOUT.varHandle(MemoryLayout.PathElement.groupElement("fFlags")).set(shfileop, 0L, (short) (FOF_ALLOWUNDO | FOF_NO_UI));
            SHFILEOPSTRUCTW_LAYOUT.varHandle(MemoryLayout.PathElement.groupElement("fAnyOperationsAborted")).set(shfileop, 0L, 0);
            SHFILEOPSTRUCTW_LAYOUT.varHandle(MemoryLayout.PathElement.groupElement("hNameMappings")).set(shfileop, 0L, MemorySegment.NULL);
            SHFILEOPSTRUCTW_LAYOUT.varHandle(MemoryLayout.PathElement.groupElement("lpszProgressTitle")).set(shfileop, 0L, MemorySegment.NULL);

            int result = (int) SHFileOperationW.invokeExact(shfileop);
            if (result != 0) {
                throw new IOException("Move to trash failed (code " + result + ")");
            }

            result = (int) SHFILEOPSTRUCTW_LAYOUT.varHandle(MemoryLayout.PathElement.groupElement("fAnyOperationsAborted")).get(shfileop, 0L);
            if (result != 0) {
                throw new IOException("Move to trash aborted");
            }
        } catch (Throwable t) {
            throw new IOException("FFM moveToTrash failed", t);
        }
    }
}
