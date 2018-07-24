/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mSearch.tool;

import com.jidesoft.utils.SystemInfo;
import mSearch.Const;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.security.CodeSource;
import java.util.ResourceBundle;
import java.util.regex.Pattern;

public class Functions {

    private static final String RBVERSION = "version";
    private static final Logger logger = LogManager.getLogger(Functions.class);
    private static final Pattern PATTERN;

    static {
        PATTERN = Pattern.compile("\\<.*?>");
    }

    public static void fastChannelCopy(final ReadableByteChannel src, final WritableByteChannel dest) throws IOException {
        final ByteBuffer buffer = ByteBuffer.allocateDirect(64 * 1024);
        while (src.read(buffer) != -1) {
            buffer.flip();
            dest.write(buffer);
            buffer.compact();
        }

        buffer.flip();

        while (buffer.hasRemaining()) {
            dest.write(buffer);
        }
    }

    public static String textLaenge(int max, String text, boolean mitte, boolean addVorne) {
        if (text.length() > max) {
            if (mitte) {
                text = text.substring(0, 25) + " .... " + text.substring(text.length() - (max - 31));
            } else {
                text = text.substring(0, max - 1);
            }
        }
        StringBuilder textBuilder = new StringBuilder(text);
        while (textBuilder.length() < max) {
            if (addVorne) {
                textBuilder.insert(0, ' ');
            } else {
                textBuilder.append(' ');
            }
        }
        text = textBuilder.toString();
        return text;
    }

    /**
     * Detect and return the currently used operating system.
     *
     * @return The enum for supported Operating Systems.
     */
    public static OperatingSystemType getOs() {
        OperatingSystemType os = OperatingSystemType.UNKNOWN;

        if (SystemInfo.isWindows()) {
            if (System.getenv("ProgramFiles(x86)") != null) {
                // win 64Bit
                os = OperatingSystemType.WIN64;
            } else if (System.getenv("ProgramFiles") != null) {
                // win 32Bit
                os = OperatingSystemType.WIN32;
            }

        } else if (SystemInfo.isLinux() || System.getProperty("os.name").toLowerCase().contains("freebsd")) {
            os = OperatingSystemType.LINUX;
        } else if (SystemInfo.isMacOSX()) {
            os = OperatingSystemType.MAC;
        }
        return os;
    }

    public static String getOsString() {
        return getOs().toString();
    }

    public static String getPathJar() {
        // liefert den Pfad der Programmdatei mit File.separator am Schluss
        String pFilePath = "version.properties";
        File propFile = new File(pFilePath);
        if (!propFile.exists()) {
            try {
                CodeSource cS = Const.class.getProtectionDomain().getCodeSource();
                File jarFile = new File(cS.getLocation().toURI().getPath());
                String jarDir = jarFile.getParentFile().getPath();
                propFile = new File(jarDir + File.separator + pFilePath);
            } catch (Exception ignored) {
            }
        } else {
            logger.debug("getPathJar() propFile does exist");
        }
        String s = StringUtils.replace(propFile.getAbsolutePath(), pFilePath, "");
        if (!s.endsWith(File.separator)) {
            s = s + File.separator;
        }
        if (s.endsWith("/lib/")) {
            // dann sind wir in der msearch-lib
            s = StringUtils.replace(s, "/lib/", "");
        }
        return s;
    }

    public static String[] getJavaVersion() {
        String[] ret = new String[4];
        ret[0] = "Vendor: " + System.getProperty("java.vendor");
        ret[1] = "VMname: " + System.getProperty("java.vm.name");
        ret[2] = "Version: " + System.getProperty("java.version");
        ret[3] = "Runtimeversion: " + System.getProperty("java.runtime.version");
        return ret;
    }

    public static String getCompileDate() {
        String propToken = "DATE";
        String msg = "";
        try {
            ResourceBundle.clearCache();
            ResourceBundle rb = ResourceBundle.getBundle(RBVERSION);
            if (rb.containsKey(propToken)) {
                msg = rb.getString(propToken);
            }
        } catch (Exception e) {
            Log.errorLog(807293847, e);
        }
        return msg;
    }

    @Deprecated
    public static String getBuildNr() {
        String TOKEN_VERSION = "VERSION";
        try {
            ResourceBundle.clearCache();
            ResourceBundle rb = ResourceBundle.getBundle(RBVERSION);
            if (rb.containsKey(TOKEN_VERSION)) {
                return new Version(rb.getString(TOKEN_VERSION)).toString();
            }
        } catch (Exception e) {
            Log.errorLog(134679898, e);
        }
        return new Version("").toString();
    }

    public static boolean istUrl(String dateiUrl) {
        return dateiUrl.startsWith("http") || dateiUrl.startsWith("www");
    }

    public static String removeHtml(final String in) {
        return PATTERN.matcher(in).replaceAll("");
    }

    public enum OperatingSystemType {

        UNKNOWN(""), WIN32("Windows"), WIN64("Windows"), LINUX("Linux"), MAC("Mac");
        private final String name;

        OperatingSystemType(String name) {
            this.name = name;
        }

        @Override
        public String toString() {
            return name;
        }
    }
}
