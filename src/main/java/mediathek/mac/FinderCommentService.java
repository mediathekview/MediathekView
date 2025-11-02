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

import com.dd.plist.BinaryPropertyListWriter;
import com.dd.plist.NSObject;
import com.dd.plist.NSString;
import com.dd.plist.PropertyListParser;

import java.io.ByteArrayOutputStream;
import java.lang.foreign.*;
import java.lang.invoke.MethodHandle;
import java.nio.file.Path;

public class FinderCommentService {
    private static final String XATTR_FINDER_COMMENT = "com.apple.metadata:kMDItemFinderComment";
    private static final String XATTR_COMMENT = "com.apple.metadata:kMDItemComment";
    private static final MethodHandle setxattr;
    private static final MethodHandle getxattr;

    static {
        try {
            var linker = Linker.nativeLinker();
            var libc = linker.defaultLookup();

            setxattr = linker.downcallHandle(libc.find("setxattr").orElseThrow(),
                    FunctionDescriptor.of(ValueLayout.JAVA_INT, // return int
                            ValueLayout.ADDRESS, // const char *path
                            ValueLayout.ADDRESS, // const char *name
                            ValueLayout.ADDRESS, // const void *value
                            ValueLayout.JAVA_LONG, // size_t size
                            ValueLayout.JAVA_INT, // uint32_t position
                            ValueLayout.JAVA_INT) // int options
            );

            getxattr = linker.downcallHandle(libc.find("getxattr").orElseThrow(),
                    FunctionDescriptor.of(ValueLayout.JAVA_LONG, // return size_t
                            ValueLayout.ADDRESS, // const char *path
                            ValueLayout.ADDRESS, // const char *name
                            ValueLayout.ADDRESS, // void *value
                            ValueLayout.JAVA_LONG, // size_t size
                            ValueLayout.JAVA_INT, // uint32_t position
                            ValueLayout.JAVA_INT) // int options
            );
        }
        catch (Throwable t) {
            throw new ExceptionInInitializerError(t);
        }
    }

    public static String cleanComment(String comment) {
        return comment.replaceAll("\\R", "");
    }

    public static void setFinderComment(Path filePath, String comment) throws Throwable {
        if (filePath.toString().isEmpty()) {
            return;
        }

        try (var arena = Arena.ofConfined(); var out = new ByteArrayOutputStream()) {
            var nsString = new NSString(cleanComment(comment));
            BinaryPropertyListWriter.write(nsString, out);
            var plistBytes = out.toByteArray();

            var pathSegment = arena.allocateFrom(filePath.toString());
            var finderCommentAttrSegment = arena.allocateFrom(XATTR_FINDER_COMMENT);
            var commentAttrSegment = arena.allocateFrom(XATTR_COMMENT);
            var value = arena.allocate(plistBytes.length);
            value.asByteBuffer().put(plistBytes);

            int result = (int) setxattr.invoke(pathSegment, finderCommentAttrSegment, value, (long) plistBytes.length, 0, 0);
            checkError(result);

            result = (int) setxattr.invoke(pathSegment, commentAttrSegment, value, (long) plistBytes.length, 0, 0);
            checkError(result);
        }
    }

    public static String getFinderComment(Path filePath) throws Throwable {
        try (var arena = Arena.ofConfined()) {
            var pathSegment = arena.allocateFrom(filePath.toString());
            var nameSegment = arena.allocateFrom(XATTR_FINDER_COMMENT);

            long size = (long) getxattr.invoke(pathSegment, nameSegment, MemorySegment.NULL, 0L, 0, 0);
            if (size <= 0)
                return null;

            var buffer = arena.allocate(size);
            long readSize = (long) getxattr.invoke(pathSegment, nameSegment, buffer, size, 0, 0);
            if (readSize != size) {
                throw new RuntimeException("Failed to read full attribute");
            }

            var plistData = buffer.toArray(ValueLayout.JAVA_BYTE);
            NSObject obj = PropertyListParser.parse(plistData);
            if (obj instanceof NSString ns) {
                return ns.getContent();
            }
            return null;
        }
    }

    private static void checkError(int result) {
        if (result != 0) {
            throw new RuntimeException("setxattr failed with error code: " + result);
        }
    }
}
