package mSearch.tool;

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the  "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
/*
 * $Id: SystemIDResolver.java 468655 2006-10-28 07:12:06Z minchau $
 */

import java.io.File;

/**
 * This class is used to resolve relative URIs and SystemID
 * strings into absolute URIs.
 *
 * <p>
 * This is a generic utility for resolving URIs, other than the
 * fact that it's declared to throw TransformerException. Please
 * see code comments for details on how resolution is performed.</p>
 */
public class AbsoluteFromRelativeURI {

    /**
     * Get an absolute URI from a given relative URI (local path).
     *
     * <p>
     * The relative URI is a local filesystem path. The path can be
     * absolute or relative. If it is a relative path, it is resolved relative
     * to the system property "user.dir" if it is available; if not (i.e. in an
     * Applet perhaps which throws SecurityException) then we just return the
     * relative path. The space and backslash characters are also replaced to
     * generate a good absolute URI.</p>
     *
     * @param localPath The relative URI to resolve
     * @return Resolved absolute URI
     */
    public static String getAbsoluteURIFromRelative(String localPath) {
        if (localPath == null || localPath.length() == 0) {
            return "";
        }

        // If the local path is a relative path, then it is resolved against
        // the "user.dir" system property.
        String absolutePath = localPath;
        if (!isAbsolutePath(localPath)) {
            try {
                absolutePath = getAbsolutePathFromRelativePath(localPath);
            } // user.dir not accessible from applet
            catch (SecurityException se) {
                return "file:" + localPath;
            }
        }

        String urlString;
        if (null != absolutePath) {
            if (absolutePath.startsWith(File.separator)) {
                urlString = "file://" + absolutePath;
            } else {
                urlString = "file:///" + absolutePath;
            }
        } else {
            urlString = "file:" + localPath;
        }

        return replaceChars(urlString);
    }

    /**
     * Return an absolute path from a relative path.
     *
     * @param relativePath A relative path
     * @return The absolute path
     */
    private static String getAbsolutePathFromRelativePath(String relativePath) {
        return new File(relativePath).getAbsolutePath();
    }

    /**
     * Return true if the local path is an absolute path.
     *
     * @param systemId The path string
     * @return true if the path is absolute
     */
    public static boolean isAbsolutePath(String systemId) {
        if (systemId == null) {
            return false;
        }
        final File file = new File(systemId);
        return file.isAbsolute();

    }

    /**
     * Replace spaces with "%20" and backslashes with forward slashes in
     * the input string to generate a well-formed URI string.
     *
     * @param str The input string
     * @return The string after conversion
     */
    private static String replaceChars(String str) {
        StringBuilder buf = new StringBuilder(str);
        int length = buf.length();
        for (int i = 0; i < length; i++) {
            char currentChar = buf.charAt(i);
            // Replace space with "%20"
            if (currentChar == ' ') {
                buf.setCharAt(i, '%');
                buf.insert(i + 1, "20");
                length = length + 2;
                i = i + 2;
            } // Replace backslash with forward slash
            else if (currentChar == '\\') {
                buf.setCharAt(i, '/');
            }
        }

        return buf.toString();
    }

}
