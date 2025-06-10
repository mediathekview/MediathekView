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
import windows.win32.ui.shell.Apis;
import windows.win32.ui.shell.SHFILEOPSTRUCTW;

import java.io.File;
import java.io.IOException;
import java.lang.foreign.Arena;
import java.lang.foreign.Linker;
import java.lang.foreign.MemorySegment;
import java.nio.charset.StandardCharsets;

public class WindowsFileUtils {

    private static final int FO_DELETE = 3;
    private static final int FOF_ALLOWUNDO = 0x40;
    private static final int FOF_NOCONFIRMATION = 0x10;
    private static final int FOF_NO_UI = FOF_NOCONFIRMATION;

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

        return arena.allocateFrom(joined.toString(), StandardCharsets.UTF_16LE);
        /*var utf16 = joined.toString().getBytes(StandardCharsets.UTF_16LE);
        //debugPrintUtf16(utf16);
        var segment = arena.allocate(utf16.length, 2); // UTF-16 requires 2-byte alignment
        segment.copyFrom(MemorySegment.ofArray(utf16));
        return segment;*/
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

            var fileOp = SHFILEOPSTRUCTW.allocate(arena);
            SHFILEOPSTRUCTW.wFunc(fileOp, FO_DELETE);
            SHFILEOPSTRUCTW.pFrom(fileOp, pFrom);
            SHFILEOPSTRUCTW.pTo(fileOp, MemorySegment.NULL);
            SHFILEOPSTRUCTW.fFlags(fileOp, (short)(FOF_ALLOWUNDO | FOF_NO_UI));
            SHFILEOPSTRUCTW.fAnyOperationsAborted(fileOp, 0);
            SHFILEOPSTRUCTW.hNameMappings(fileOp, MemorySegment.NULL);
            SHFILEOPSTRUCTW.lpszProgressTitle(fileOp, MemorySegment.NULL);

            var errorStateLayout = Linker.Option.captureStateLayout();
            var errorState = arena.allocate(errorStateLayout);
            var result = Apis.SHFileOperationW(errorState, fileOp);
            if (result != 0) {
                throw new IOException("Move to trash failed (code " + result + ")");
            }

            if (SHFILEOPSTRUCTW.fAnyOperationsAborted(fileOp) != 0) {
                throw new IOException("Move to trash aborted");
            }
        } catch (Throwable t) {
            throw new IOException("FFM moveToTrash failed", t);
        }
    }
}
