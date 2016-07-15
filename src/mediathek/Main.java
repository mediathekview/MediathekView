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
package mediathek;

import com.jidesoft.utils.SystemInfo;
import com.jidesoft.utils.ThreadCheckingRepaintManager;
import java.awt.EventQueue;
import java.awt.GraphicsEnvironment;
import java.awt.Toolkit;
import java.io.File;
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import javafx.application.Platform;
import javax.swing.JOptionPane;
import javax.swing.RepaintManager;
import mSearch.Config;
import mSearch.tool.Log;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.mac.MediathekGuiMac;
import mediathek.config.Konstanten;
import static mediathek.tool.MVFunctionSys.startMeldungen;
import mSearch.tool.SingleInstance;

public class Main {

    public enum StartupMode {

        GUI, AUTO, FASTAUTO
    }

    /**
     * Ensures that old film lists in .mediathek directory get deleted because they were moved to
     * ~/Library/Caches/MediathekView
     */
    private static void cleanupOsxFiles() {
        try {
            Path oldFilmList = Paths.get(Daten.getSettingsDirectory_String() + File.separator + Konstanten.JSON_DATEI_FILME);
            Files.deleteIfExists(oldFilmList);
        } catch (Exception ignored) {
        }
    }

    /*
     * Aufruf:
     * java -jar Mediathek [Pfad zur Konfigdatei, sonst homeverzeichnis] [Schalter]
     *
     * Programmschalter:
     *
     * -M Fenster maximiert starten
     * -A Automodus
     * -noGui ohne GUI starten und die Filmliste laden
     *
     * */
    /**
     * @param args the command line arguments
     */
    public static void main(final String args[]) {
        StartupMode state = StartupMode.GUI;

        try {
            String tmp = System.getProperty("http.proxyUser", System.getProperty("https.proxyUser"));
            if (tmp != null) {
                Authenticator.setDefault(new Authenticator() {
                    @Override
                    protected PasswordAuthentication getPasswordAuthentication() {
                        String proxyUser = System.getProperty("http.proxyUser");
                        String proxyPassword = System.getProperty("http.proxyPassword");
                        return new PasswordAuthentication(proxyUser, proxyPassword.toCharArray());
                    }
                });
                SysMsg.sysMsg("Proxy Authentication: (" + System.getProperty("http.proxyUser") + ")");
            } else {
                SysMsg.sysMsg("Proxy Authentication: not configured");
            }

        } catch (SecurityException se) {
            SysMsg.sysMsg("Proxy Authentication: cannot access proxyUser / proxyPassword" + se.toString());
        }

        if (args != null) {
            for (String s : args) {
                s = s.toLowerCase();
                switch (s) {
                    case "-auto":
                        state = StartupMode.AUTO;
                        break;

                    case "-fastauto":
                        state = StartupMode.FASTAUTO;
                        break;

                    case "-v":
                        EventQueue.invokeLater(() -> {
                            startMeldungen();
                            SysMsg.sysMsg("Systemmeldung");
                            Log.errorLog(100000000, "Fehlermeldung");
                            Log.endMsg();
                            System.exit(0);
                        });
                        break;

                    case "-d":
                        Daten.debug = true;
                        Config.debug = true;

//                        EventQueue.invokeLater(() -> {
//                            // zum Test
//                            Log.startMeldungen();
//                            Log.systemMeldung("Test 0");
//                            MSLog.fehlerMeldung(100000000, "Test 1");
//                            MSLog.fehlerMeldung(200000000, "Test 2");
//                            MSLog.fehlerMeldung(0, "Test 3");
//                        });

                        break;

                    case "-m":
                        Daten.startMaximized = true;
                        break;
                }
            }
        }

        /*
         If user tries to start MV from command-line without proper options,
         instead of crashing while trying to open Swing windows, just change to CLI mode and warn the user.
         */
        if (GraphicsEnvironment.isHeadless() && (state == StartupMode.GUI)) {
            System.err.println("MediathekView wurde nicht als Kommandozeilenprogramm gestartet.");
            System.err.println("Startmodus wurde auf -auto geändert.");
            System.err.println();
            state = StartupMode.AUTO;
        }

        switch (state) {
            case AUTO:
                new MediathekAuto(args).starten();
                break;

            case FASTAUTO:
                final MediathekAuto mvAuto = new MediathekAuto(args);
                mvAuto.setFastAuto(true);
                mvAuto.starten();
                break;

            case GUI:
                EventQueue.invokeLater(() -> {
                    //JavaFX stuff
                    Platform.setImplicitExit(false);

                    if (SystemInfo.isMacOSX()) {
                        System.setProperty("apple.laf.useScreenMenuBar", "true");
                        cleanupOsxFiles();
                    }

                    if (Daten.debug) {
                        // use for debugging EDT violations
                        RepaintManager.setCurrentManager(new ThreadCheckingRepaintManager());

                        if (SystemInfo.isMacOSX()) {
                            //prevent startup of multiple instances...useful during debugging :(
                            SingleInstance singleInstanceWatcher = new SingleInstance();
                            if (singleInstanceWatcher.isAppAlreadyActive()) {
                                JOptionPane.showMessageDialog(null, "MediathekView is already running!");
                                //System.exit(1);
                            }
                        }
                    }
                    if (SystemInfo.isMacOSX()) {
                        new MediathekGuiMac(args).setVisible(true);
                    } else {
                        if (SystemInfo.isLinux()) {
                            try {
                                Toolkit xToolkit = Toolkit.getDefaultToolkit();
                                java.lang.reflect.Field awtAppClassNameField = xToolkit.getClass().getDeclaredField("awtAppClassName");
                                awtAppClassNameField.setAccessible(true);
                                awtAppClassNameField.set(xToolkit, "MediathekView");
                            } catch (Exception ignored) {
                                System.err.println("Couldn't set awtAppClassName");
                            }
                        }
                        new MediathekGui(args).setVisible(true);
                    }
                });
                break;
        }
    }
}
