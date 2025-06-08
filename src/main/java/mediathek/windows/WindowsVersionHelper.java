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

import java.lang.foreign.*;

public class WindowsVersionHelper {
    private static final int VER_MINORVERSION = 0x0000001;
    private static final int VER_MAJORVERSION = 0x0000002;
    private static final int VER_SERVICEPACKMAJOR = 0x0000020;
    private static final byte VER_GREATER_EQUAL = 3;

    private static final GroupLayout OSVERSIONINFOEXW_LAYOUT = MemoryLayout.structLayout(
            ValueLayout.JAVA_INT.withName("dwOSVersionInfoSize"),
            ValueLayout.JAVA_INT.withName("dwMajorVersion"),
            ValueLayout.JAVA_INT.withName("dwMinorVersion"),
            ValueLayout.JAVA_INT.withName("dwBuildNumber"),
            ValueLayout.JAVA_INT.withName("dwPlatformId"),
            MemoryLayout.sequenceLayout(128, ValueLayout.JAVA_CHAR).withName("szCSDVersion"),
            ValueLayout.JAVA_SHORT.withName("wServicePackMajor"),
            ValueLayout.JAVA_SHORT.withName("wServicePackMinor"),
            ValueLayout.JAVA_SHORT.withName("wSuiteMask"),
            ValueLayout.JAVA_BYTE.withName("wProductType"),
            ValueLayout.JAVA_BYTE.withName("wReserved")
    ).withName("OSVERSIONINFOEXW");

    private static final long OSVERSIONINFOEXW_STRUCT_SIZE = OSVERSIONINFOEXW_LAYOUT.byteSize();


    public static boolean IsWindows10OrGreater() throws Throwable {
        return isWindowsVersionOrGreater(10, 0, 0);
    }

    public static boolean IsWindows11OrGreater() throws Throwable {
        return isWindowsVersionOrGreater(11, 0, 0);
    }

    protected static boolean isWindowsVersionOrGreater(int major, int minor, int servicePackMajor) throws Throwable {
        try (var arena = Arena.ofConfined()) {
            SymbolLookup kernel32 = SymbolLookup.libraryLookup("kernel32.dll", Arena.global());

            var nativeLinker = Linker.nativeLinker();
            var MH_VerSetConditionMask = nativeLinker.downcallHandle(
                    kernel32.find("VerSetConditionMask").orElseThrow(() -> new UnsatisfiedLinkError("VerSetConditionMask not found")),
                    FunctionDescriptor.of(ValueLayout.JAVA_LONG, ValueLayout.JAVA_LONG, ValueLayout.JAVA_INT, ValueLayout.JAVA_BYTE)
            );

            var MH_VerifyVersionInfoW = nativeLinker.downcallHandle(
                    kernel32.find("VerifyVersionInfoW").orElseThrow(() -> new UnsatisfiedLinkError("VerifyVersionInfoW not found")),
                    FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.JAVA_INT, ValueLayout.JAVA_LONG)
            );

            var VH_dwOSVersionInfoSize = OSVERSIONINFOEXW_LAYOUT.varHandle(MemoryLayout.PathElement.groupElement("dwOSVersionInfoSize"));
            var VH_dwMajorVersion = OSVERSIONINFOEXW_LAYOUT.varHandle(MemoryLayout.PathElement.groupElement("dwMajorVersion"));
            var VH_dwMinorVersion = OSVERSIONINFOEXW_LAYOUT.varHandle(MemoryLayout.PathElement.groupElement("dwMinorVersion"));
            var VH_wServicePackMajor = OSVERSIONINFOEXW_LAYOUT.varHandle(MemoryLayout.PathElement.groupElement("wServicePackMajor"));

            var osvi = arena.allocate(OSVERSIONINFOEXW_LAYOUT);
            VH_dwOSVersionInfoSize.set(osvi, 0L, (int) OSVERSIONINFOEXW_STRUCT_SIZE);
            VH_dwMajorVersion.set(osvi, 0L, major);
            VH_dwMinorVersion.set(osvi, 0L, minor);
            VH_wServicePackMajor.set(osvi, 0L, (short) servicePackMajor);

            var conditionMask = 0L;
            conditionMask = (long) MH_VerSetConditionMask.invokeExact(conditionMask, VER_MAJORVERSION, VER_GREATER_EQUAL);
            conditionMask = (long) MH_VerSetConditionMask.invokeExact(conditionMask, VER_MINORVERSION, VER_GREATER_EQUAL);
            conditionMask = (long) MH_VerSetConditionMask.invokeExact(conditionMask, VER_SERVICEPACKMAJOR, VER_GREATER_EQUAL);

            int result = (int) MH_VerifyVersionInfoW.invokeExact(osvi, VER_MAJORVERSION | VER_MINORVERSION | VER_SERVICEPACKMAJOR, conditionMask);

            return result != 0;
        }
    }
}