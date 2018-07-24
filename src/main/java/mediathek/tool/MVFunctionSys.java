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

import mSearch.tool.Log;
import mediathek.Main;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.security.CodeSource;

import static mSearch.tool.Log.LILNE;

public class MVFunctionSys {

    public static synchronized void startMeldungen() {
        Log.versionMsg(MVFunctionSys.getProgName());
        logger.info("Programmpfad: " + MVFunctionSys.getPathJar());
        logger.info("Verzeichnis Einstellungen: " + Daten.getSettingsDirectory_String());
        logger.info("");
        logger.info(LILNE);
        logger.info("");
        logger.info("");
    }

    private static final Logger logger = LogManager.getLogger(MVFunctionSys.class);

    /**
     * Retrieve the path to the program jar file.
     *
     * @return The program jar file path with a separator added.
     */
    public static String getPathJar() {
        // macht Probleme bei Win und Netzwerkpfaden, liefert dann Absolute Pfade zB. \\VBOXSVR\share\Mediathek\...
        String pFilePath = "pFile";
        File propFile = new File(pFilePath);
        if (!propFile.exists()) {
            try {
                CodeSource cS = Main.class.getProtectionDomain().getCodeSource();
                File jarFile = new File(cS.getLocation().toURI().getPath());
                String jarDir = jarFile.getParentFile().getPath();
                propFile = new File(jarDir + File.separator + pFilePath);
            } catch (Exception ignored) {
            }
        }
        String s = StringUtils.replace(propFile.getAbsolutePath(), pFilePath, "");
        if (!s.endsWith(File.separator)) {
            s = s + File.separator;
        }
        return s;
    }

    public static String pathProgramIcons() {
        return getPathJar() + Konstanten.VERZEICHNIS_PROGRAMM_ICONS;
    }

    public static String pathSenderIcons() {
        return getPathJar() + Konstanten.VERZEICHNIS_SENDER_ICONS;
    }

    public static String getProgVersionString() {
        return Konstanten.PROGRAMMNAME + ' ' + Konstanten.MVVERSION;
    }

    public static String getProgName() {
        return Konstanten.PROGRAMMNAME + ' ' + Konstanten.MVVERSION;
    }
}
