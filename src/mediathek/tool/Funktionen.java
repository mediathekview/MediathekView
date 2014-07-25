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
package mediathek.tool;

import com.jidesoft.utils.SystemInfo;
import java.io.File;
import java.security.CodeSource;
import java.util.ResourceBundle;
import mediathek.Main;
import mediathek.controller.Log;

public class Funktionen {

    public static final int OS_UNKNOWN = 0;
    public static final int OS_WIN_32BIT = 1;
    public static final int OS_WIN_64BIT = 2;
    public static final int OS_LINUX = 3;
    public static final int OS_MAC = 4;
    public static final String OS_UNKNOWN_STRING = "";
    public static final String OS_WIN_32BIT_STRING = "Windows";
    public static final String OS_WIN_64BIT_STRING = "Windows";
    public static final String OS_LINUX_STRING = "Linux";
    public static final String OS_MAC_STRING = "Mac";

    public static boolean isOsx() {
        return SystemInfo.isMacOSX();
    }

    public static int getOs() {
        int os = OS_UNKNOWN;
        if (System.getProperty("os.name").toLowerCase().contains("windows")) {
            if (System.getenv("ProgramFiles") != null) {
                // win 32Bit
                os = OS_WIN_32BIT;
            } else if (System.getenv("ProgramFiles(x86)") != null) {
                // win 64Bit
                os = OS_WIN_64BIT;
            }
        } else if (System.getProperty("os.name").toLowerCase().contains("linux")) {
            os = OS_LINUX;
        } else if (System.getProperty("os.name").toLowerCase().contains("mac")) {
            os = OS_MAC;
        }
        return os;
    }

    public static String getOsString() {
        String os = OS_UNKNOWN_STRING;
        if (System.getProperty("os.name").toLowerCase().contains("windows")) {
            if (System.getenv("ProgramFiles") != null) {
                // win 32Bit
                os = OS_WIN_32BIT_STRING;
            } else if (System.getenv("ProgramFiles(x86)") != null) {
                // win 64Bit
                os = OS_WIN_64BIT_STRING;
            }
        } else if (System.getProperty("os.name").toLowerCase().contains("linux")) {
            os = OS_LINUX_STRING;
        } else if (System.getProperty("os.name").toLowerCase().contains("mac")) {
            os = OS_MAC_STRING;
        }
        return os;
    }

    public static String getPathJar() {
        // liefert den Pfad der Programmdatei mit File.separator am Schluss
        String pFilePath = "pFile";
        File propFile = new File(pFilePath);
        if (!propFile.exists()) {
            try {
                CodeSource cS = Main.class.getProtectionDomain().getCodeSource();
                File jarFile = new File(cS.getLocation().toURI().getPath());
                String jarDir = jarFile.getParentFile().getPath();
                propFile = new File(jarDir + File.separator + pFilePath);
            } catch (Exception ex) {
            }
        }
        String s = propFile.getAbsolutePath().replace(pFilePath, "");
        if (!s.endsWith(File.separator)) {
            s = s + File.separator;
        }
        return s;
    }

    public static String pathProgramIcons() {
        return getPathJar() + Konstanten.VERZEICNHISS_PROGRAMM_ICONS;
    }

    public static String pathSenderIcons() {
        return getPathJar() + Konstanten.VERZEICNHISS_SENDER_ICONS;
    }

    public static String getProgVersionString() {
        return Konstanten.PROGRAMMNAME + " " + Konstanten.VERSION + "  [Buildnummer: " + getBuildNr() + "]";
    }

    public static String[] getJavaVersion() {
        String[] ret = new String[4];
        int i = 0;
        ret[i++] = "Vendor: " + System.getProperty("java.vendor");
        ret[i++] = "VMname: " + System.getProperty("java.vm.name");
        ret[i++] = "Version: " + System.getProperty("java.version");
        ret[i++] = "Runtimeversion: " + System.getProperty("java.runtime.version");
        return ret;
    }

    public static String getCompileDate() {
        final ResourceBundle rb;
        String propToken = "DATE";
        String msg = "";
        try {
            ResourceBundle.clearCache();
            rb = ResourceBundle.getBundle("version");
            msg = rb.getString(propToken);
        } catch (Exception e) {
            Log.fehlerMeldung(807293847, Log.FEHLER_ART_PROG, Funktionen.class.getName(), e);
        }
        return msg;
    }

    public static String getBuildNr() {
        final ResourceBundle rb;
        String propToken = "BUILD";
        String msg = "";
        try {
            ResourceBundle.clearCache();
            rb = ResourceBundle.getBundle("version");
            msg = rb.getString(propToken);
        } catch (Exception e) {
            Log.fehlerMeldung(134679898, Log.FEHLER_ART_PROG, Funktionen.class.getName(), e);
        }
        return msg;
    }

//    public enum ReleaseType { DEBUG, RELEASE}
//    public static ReleaseType getReleaseType()
//    {
//        ReleaseType releaseType;
//        try {
//            ResourceBundle.clearCache();
//            ResourceBundle rb = ResourceBundle.getBundle("version");
//            String msg = rb.getString("TYPE");
//            switch(msg) {
//                case "DEBUG":
//                    releaseType = ReleaseType.DEBUG;
//                    break;
//                default:
//                    releaseType = ReleaseType.RELEASE;
//                    break;
//            }
//        } catch (Exception ex) {
//            //in case of an exception always pretend we are in Release mode...
//            ex.printStackTrace();
//            releaseType = ReleaseType.DEBUG;////////////////
//        }
//
//        return releaseType;
//    }
}
