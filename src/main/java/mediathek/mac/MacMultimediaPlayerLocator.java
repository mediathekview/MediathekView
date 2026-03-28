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

import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.lang.foreign.*;
import java.lang.invoke.MethodHandle;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;

public final class MacMultimediaPlayerLocator {

    private static final Linker LINKER = Linker.nativeLinker();
    private static final int kCFStringEncodingUTF8 = 0x08000100;
    private static final int kCFURLPOSIXPathStyle = 0;
    private static final MethodHandle CFStringCreateWithCString;
    private static final MethodHandle CFStringGetCString;
    private static final MethodHandle CFRelease;
    private static final MethodHandle LSCopyApplicationURLsForBundleIdentifier;
    private static final MethodHandle CFArrayGetCount;
    private static final MethodHandle CFArrayGetValueAtIndex;
    private static final MethodHandle CFURLCopyFileSystemPath;
    private static final Logger LOG = LogManager.getLogger();

    static {
        Arena global = Arena.global();

        var coreFoundation =
                SymbolLookup.libraryLookup("/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation", global);

        var coreServices =
                SymbolLookup.libraryLookup("/System/Library/Frameworks/CoreServices.framework/CoreServices", global);

        try {
            CFStringCreateWithCString = LINKER.downcallHandle(
                    coreFoundation.find("CFStringCreateWithCString").orElseThrow(),
                    FunctionDescriptor.of(
                            ValueLayout.ADDRESS,
                            ValueLayout.ADDRESS,
                            ValueLayout.ADDRESS,
                            ValueLayout.JAVA_INT
                    ));

            CFStringGetCString = LINKER.downcallHandle(coreFoundation.find("CFStringGetCString").orElseThrow(),
                    FunctionDescriptor.of(
                            ValueLayout.JAVA_INT,
                            ValueLayout.ADDRESS,
                            ValueLayout.ADDRESS,
                            ValueLayout.JAVA_LONG,
                            ValueLayout.JAVA_INT
                    ));

            CFRelease = LINKER.downcallHandle(coreFoundation.find("CFRelease").orElseThrow(),
                    FunctionDescriptor.ofVoid(ValueLayout.ADDRESS));

            LSCopyApplicationURLsForBundleIdentifier = LINKER.downcallHandle(
                    coreServices.find("LSCopyApplicationURLsForBundleIdentifier").orElseThrow(),
                    FunctionDescriptor.of(
                            ValueLayout.ADDRESS,
                            ValueLayout.ADDRESS,
                            ValueLayout.ADDRESS
                    ));

            CFArrayGetCount = LINKER.downcallHandle(coreFoundation.find("CFArrayGetCount").orElseThrow(),
                    FunctionDescriptor.of(
                            ValueLayout.JAVA_LONG,
                            ValueLayout.ADDRESS
                    ));

            CFArrayGetValueAtIndex = LINKER.downcallHandle(coreFoundation.find("CFArrayGetValueAtIndex").orElseThrow(),
                    FunctionDescriptor.of(
                            ValueLayout.ADDRESS,
                            ValueLayout.ADDRESS,
                            ValueLayout.JAVA_LONG
                    ));

            CFURLCopyFileSystemPath = LINKER.downcallHandle(
                    coreFoundation.find("CFURLCopyFileSystemPath").orElseThrow(),
                    FunctionDescriptor.of(
                            ValueLayout.ADDRESS,
                            ValueLayout.ADDRESS,
                            ValueLayout.JAVA_INT
                    ));
        }
        catch (Throwable t) {
            throw new ExceptionInInitializerError(t);
        }
    }

    private MacMultimediaPlayerLocator() {
    }

    public static Optional<Path> findIinaPlayer() {
        return findAppBundle("com.colliderli.iina").map(p -> p.resolve("Contents/MacOS/iina"));
    }

    public static Optional<Path> findVlcPlayer() {
        return findAppBundle("org.videolan.vlc").map(p -> p.resolve("Contents/MacOS/VLC"));
    }

    /**
     * Resolve the VLC.app bundle using macOS Launch Services and FFM.
     *
     * @return Optional<Path> to the VLC.app bundle.
     */
    private static Optional<Path> findAppBundle(String bundleId) {
        if (!SystemUtils.IS_OS_MAC_OSX) {
            return Optional.empty();
        }

        try (Arena arena = Arena.ofConfined()) {
            var cBundleId = arena.allocateFrom(bundleId);
            var cfBundleId = (MemorySegment) CFStringCreateWithCString.invoke(
                    MemorySegment.NULL, cBundleId, kCFStringEncodingUTF8);
            if (cfBundleId.equals(MemorySegment.NULL)) {
                return Optional.empty();
            }

            try {
                var outErrorPtr = arena.allocate(ValueLayout.ADDRESS);

                var cfArray = (MemorySegment) LSCopyApplicationURLsForBundleIdentifier.invoke(
                        cfBundleId,
                        outErrorPtr
                );
                if (cfArray.equals(MemorySegment.NULL)) {
                    return Optional.empty();
                }

                try {
                    long count = (long) CFArrayGetCount.invoke(cfArray);
                    if (count == 0) {
                        return Optional.empty();
                    }

                    var cfUrl = (MemorySegment) CFArrayGetValueAtIndex.invoke(cfArray, 0L);
                    if (cfUrl.equals(MemorySegment.NULL)) {
                        return Optional.empty();
                    }

                    var cfPathStr = (MemorySegment) CFURLCopyFileSystemPath.invoke(
                            cfUrl,
                            kCFURLPOSIXPathStyle
                    );
                    if (cfPathStr.equals(MemorySegment.NULL)) {
                        return Optional.empty();
                    }

                    try {
                        long bufferSize = 4096;
                        var buffer = arena.allocate(bufferSize);

                        int ok = (int) CFStringGetCString.invoke(
                                cfPathStr,
                                buffer,
                                bufferSize,
                                kCFStringEncodingUTF8
                        );

                        if (ok == 0) {
                            return Optional.empty();
                        }

                        var pathStr = buffer.getString(0);
                        return Optional.of(Paths.get(pathStr));

                    }
                    finally {
                        CFRelease.invoke(cfPathStr);
                    }

                }
                finally {
                    CFRelease.invoke(cfArray);
                }

            }
            finally {
                CFRelease.invoke(cfBundleId);
            }

        }
        catch (Throwable t) {
            LOG.error("Error while resolving VLC bundle via Launch Services / FFM.", t);
            return Optional.empty();
        }
    }
}
