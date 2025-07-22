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

import com.dd.plist.NSObject;
import com.dd.plist.NSString;
import com.dd.plist.PropertyListParser;
import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.file.Path;

public class FinderCommentService {
    private static final String ATTR_FINDER_COMMENT = "com.apple.metadata:kMDItemFinderComment";

    public static void setFinderComment(Path filePath, String comment) throws Exception {
        try (var out = new ByteArrayOutputStream()) {
            var nsString = new NSString(comment);
            PropertyListParser.saveAsBinary(nsString, out);
            byte[] plistBytes = out.toByteArray();


            Pointer data = new Memory(plistBytes.length);
            data.write(0, plistBytes, 0, plistBytes.length);

            int result = LibC.INSTANCE.setxattr(filePath.toString(), ATTR_FINDER_COMMENT, data, plistBytes.length, 0, 0);

            if (result != 0) {
                throw new RuntimeException("setxattr failed: " + Native.getLastError());
            }
        }
    }

    public static String getFinderComment(Path filePath) throws Exception {
        long size = FinderCommentService.LibC.INSTANCE.getxattr(filePath.toString(), ATTR_FINDER_COMMENT, null, 0, 0, 0);
        if (size <= 0)
            return null;

        Pointer buffer = new Memory(size);
        long readSize = FinderCommentService.LibC.INSTANCE.getxattr(filePath.toString(), ATTR_FINDER_COMMENT, buffer, size, 0, 0);
        if (readSize != size) {
            throw new RuntimeException("Failed to read full attribute");
        }

        byte[] plistData = buffer.getByteArray(0, (int) size);
        NSObject obj = PropertyListParser.parse(new ByteArrayInputStream(plistData));
        if (obj instanceof NSString ns) {
            return ns.getContent();
        }

        return null;
    }

    public interface LibC extends com.sun.jna.Library {
        LibC INSTANCE = Native.load("c", LibC.class);

        int setxattr(String path, String name, Pointer value, long size, int position, int options);

        long getxattr(String path, String name, Pointer value, long size, int position, int options);
    }
}
