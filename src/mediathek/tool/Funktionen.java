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

import java.io.File;
import java.security.CodeSource;
import java.util.ResourceBundle;
import mediathek.Main;

public class Funktionen {

    public static String getPathJar() {
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
        return propFile.getAbsolutePath().replace(pFilePath, "");
    }

    public static String getCompileDate() {
//        String ret = "";
//        try {
//            Date d = new Date(Main.class.getResource("Main.class").openConnection().getLastModified());
//            ret = Konstanten.PROGRAMMNAME + " " + Konstanten.VERSION + "  [Buildnummer: " + getBuildNr() + "] - Compiled: " + new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(d);
//        } catch (Exception ex) {
//            Log.fehlerMeldung(569614756, "Log.getCompileDate: ", ex);
//        }
//        return ret;
        final ResourceBundle rb;
        String propToken = "BUILDDATE";
        String msg = "";
        try {
            ResourceBundle.clearCache();
            rb = ResourceBundle.getBundle("version");
            msg = rb.getString(propToken);
        } catch (Exception e) {
            System.err.println(e.getMessage());
            System.err.println("Token " + propToken + " not in Propertyfile!");
        }
        return Konstanten.PROGRAMMNAME + " " + Konstanten.VERSION + "  [Buildnummer: " + getBuildNr() + "] - Compiled: " + msg;
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
            System.err.println(e.getMessage());
            System.err.println("Token " + propToken + " not in Propertyfile!");
        }
        return msg;
    }
}
