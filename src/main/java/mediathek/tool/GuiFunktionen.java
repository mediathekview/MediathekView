package mediathek.tool;

import ca.odell.glazedlists.swing.DefaultEventComboBoxModel;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.config.MVConfig.Configs;
import mediathek.daten.ListeFilme;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.Functions.OperatingSystemType;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.StringSelection;
import java.awt.event.InputEvent;
import java.io.File;

import static mediathek.tool.Functions.getOs;

public class GuiFunktionen extends MVFunctionSys {

    private final static int WIN_MAX_PATH_LENGTH = 250;
    private final static int X_MAX_NAME_LENGTH = 255;
    /**
     * legacy constant, used internally only
     */
    private static final int UPDATE_FILME_AUS = 0;
    /**
     * legacy constant, used internally only
     */
    private static final int UPDATE_FILME_AUTO = 2;

    public static void updateGui(MediathekGui mediathekGui) {
        try {
            SwingUtilities.updateComponentTreeUI(mediathekGui);
            for (Frame f : Frame.getFrames()) {
                SwingUtilities.updateComponentTreeUI(f);
                for (Window w : f.getOwnedWindows()) {
                    SwingUtilities.updateComponentTreeUI(w);
                }
            }
        } catch (Exception ignored) {
        }

    }

    public static void copyToClipboard(String s) {
        Toolkit.getDefaultToolkit().getSystemClipboard().setContents(new StringSelection(s), null);
    }

    /**
     * Center a component (e.g. Dialog) on screen
     *
     * @param c        The reference component
     * @param absolute if true, use absolute position, otherwise relative
     */
    public static void centerOnScreen(final Component c, final boolean absolute) {
        final int width = c.getWidth();
        final int height = c.getHeight();
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        int x = (screenSize.width / 2) - (width / 2);
        int y = (screenSize.height / 2) - (height / 2);
        if (!absolute) {
            x /= 2;
            y /= 2;
        }
        c.setLocation(x, y);
    }

    public static void getSize(Configs nr, JFrame jFrame) {
        if (jFrame != null) {
            MVConfig.add(nr, jFrame.getSize().width + ":"
                    + jFrame.getSize().height + ':'
                    + jFrame.getLocation().x + ':'
                    + jFrame.getLocation().y);
        }
    }

    public static void getSize(Configs nr, JDialog jDialog) {
        if (jDialog != null) {
            MVConfig.add(nr, jDialog.getSize().width + ":"
                    + jDialog.getSize().height + ':'
                    + jDialog.getLocation().x + ':'
                    + jDialog.getLocation().y);
        }
    }

    public static boolean setSize(Configs nr, JDialog jDialog, Frame relativFrame) {
        boolean ret = false;
        int breite, hoehe, posX, posY;
        breite = 0;
        hoehe = 0;
        posX = 0;
        posY = 0;
        String[] arr = MVConfig.get(nr).split(":");
        try {
            if (arr.length == 4) {
                breite = Integer.parseInt(arr[0]);
                hoehe = Integer.parseInt(arr[1]);
                posX = Integer.parseInt(arr[2]);
                posY = Integer.parseInt(arr[3]);
            }
        } catch (Exception ex) {
            breite = 0;
            hoehe = 0;
            posX = 0;
            posY = 0;
        }
        if (breite > 0 && hoehe > 0) {
            jDialog.setSize(new Dimension(breite, hoehe));
            ret = true;
        }
        if (posX > 0 && posY > 0) {
            jDialog.setLocation(posX, posY);
        } else if (relativFrame != null) {
            jDialog.setLocationRelativeTo(relativFrame);
        }
        return ret;
    }

    public static String addsPfad(String pfad1, String pfad2) {
        String ret = concatPaths(pfad1, pfad2);
        if (ret.isEmpty()) {
            Log.errorLog(283946015, pfad1 + " - " + pfad2);
        }
        return ret;
    }

    public static String concatPaths(String pfad1, String pfad2) {
        String ret;

        if (pfad1 == null || pfad2 == null) {
            return "";
        }
        if (pfad1.isEmpty() || pfad2.isEmpty()) {
            return pfad1 + pfad2;
        }

        if (pfad1.endsWith(File.separator)) {
            ret = pfad1.substring(0, pfad1.length() - 1);
        } else {
            ret = pfad1;
        }
        if (pfad2.charAt(0) == File.separatorChar) {
            ret += pfad2;
        } else {
            ret += File.separator + pfad2;
        }
        return ret;
    }

    public static String[] checkLengthPath(String[] pathName) {
        if (SystemUtils.IS_OS_WINDOWS) {
            // in Win dürfen die Pfade nicht länger als 260 Zeichen haben (für die Infodatei kommen noch ".txt" dazu)
            if ((pathName[0].length() + 10) > WIN_MAX_PATH_LENGTH) {
                // es sollen für den Dateinamen mind. 10 Zeichen bleiben
                Log.errorLog(102036598, "Pfad zu lang: " + pathName[0]);
                pathName[0] = GuiFunktionen.getHomePath();
            }
            if ((pathName[0].length() + pathName[1].length()) > WIN_MAX_PATH_LENGTH) {
                Log.errorLog(902367369, "Name zu lang: " + pathName[0]);
                int maxNameL = WIN_MAX_PATH_LENGTH - pathName[0].length();
                pathName[1] = cutName(pathName[1], maxNameL);
            }
        } else // für X-Systeme
            if ((pathName[1].length()) > X_MAX_NAME_LENGTH) {
                Log.errorLog(823012012, "Name zu lang: " + pathName[1]);
                pathName[1] = cutName(pathName[1], X_MAX_NAME_LENGTH);
            }
        return pathName;
    }

    public static String cutName(String name, int length) {
        if (name.length() > length) {
            name = name.substring(0, length - 4) + name.substring(name.length() - 4);
        }
        return name;
    }

    public static boolean istUrl(String dateiUrl) {
        //return dateiUrl.startsWith("http") ? true : false || dateiUrl.startsWith("www") ? true : false;
        return dateiUrl.startsWith("http") || dateiUrl.startsWith("www");
    }

    public static String getDateiName(String pfad) {
        //Dateinamen einer URL extrahieren
        String ret = "";
        if (pfad != null) {
            if (!pfad.isEmpty()) {
                ret = pfad.substring(pfad.lastIndexOf('/') + 1);
            }
        }
        if (ret.contains("?")) {
            ret = ret.substring(0, ret.indexOf('?'));
        }
        if (ret.contains("&")) {
            ret = ret.substring(0, ret.indexOf('&'));
        }
        if (ret.isEmpty()) {
            Log.errorLog(395019631, pfad);
        }
        return ret;
    }

    public static String getSuffixFromUrl(String pfad) {
        // Suffix einer URL extrahieren
        // "http://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av.m3u8?e=b471643725c47acd"
        String ret = "";
        if (pfad != null) {
            if (!pfad.isEmpty() && pfad.contains(".")) {
                ret = pfad.substring(pfad.lastIndexOf('.') + 1);
            }
        }
        if (ret.isEmpty()) {
            Log.errorLog(969871236, pfad);
        }
        if (ret.contains("?")) {
            ret = ret.substring(0, ret.indexOf('?'));
        }
        if (ret.length() > 5) {
            // dann ist was faul
            ret = "---";
            Log.errorLog(821397046, pfad);
        }
        return ret;
    }

    public static String getFileNameWithoutSuffix(String pfad) {
        // Suffix einer URL extrahieren
        // "http://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av.m3u8?e=b471643725c47acd"
        // FILENAME.SUFF
        String ret = "";
        if (pfad != null) {
            if (!pfad.isEmpty() && pfad.contains(".")) {
                ret = pfad.substring(0, pfad.lastIndexOf('.'));
            }
        }
        if (ret.isEmpty()) {
            ret = pfad;
            Log.errorLog(945123647, pfad);
        }
        return ret;
    }

    /**
     * Return the path to the user´s home directory.
     *
     * @return String to the user´s home directory.
     */
    public static String getHomePath() {
        return System.getProperty("user.home");
    }

    /**
     * Liefert den Standardpfad für Downloads.
     *
     * @return Standardpfad zu den Downloads.
     */
    public static String getStandardDownloadPath() {
        if (getOs() == OperatingSystemType.MAC) {
            return addsPfad(getHomePath(), "Downloads");
        } else {
            return addsPfad(getHomePath(), Konstanten.VERZEICHNIS_DOWNLOADS);
        }
    }

    public static ComboBoxModel<String> getSenderListComboBoxModel(ListeFilme listeFilme) {
        DefaultEventComboBoxModel<String> senderModel = new DefaultEventComboBoxModel<>(new SenderList(listeFilme.getBaseSenderList()));
        senderModel.setSelectedItem("");

        return senderModel;
    }

    /**
     * Maps the "command" key to the correspondig icon based on operating system.
     *
     * @return an InputEvent modifier based on operating system.
     */
    public static int getPlatformControlKey() {
        int result;

        if (getOs() == OperatingSystemType.MAC) {
            result = InputEvent.META_DOWN_MASK;
        } else {
            result = InputEvent.CTRL_DOWN_MASK;
        }

        return result;
    }

    public static FilmListUpdateType getImportArtFilme() {
        FilmListUpdateType result;

        int ret;
        try {
            ret = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_IMPORT_ART_FILME));
        } catch (Exception ex) {
            MVConfig.add(MVConfig.Configs.SYSTEM_IMPORT_ART_FILME, String.valueOf(UPDATE_FILME_AUTO));
            ret = UPDATE_FILME_AUTO;
        }

        if (ret == UPDATE_FILME_AUS) {
            result = FilmListUpdateType.MANUAL;
        } else {
            result = FilmListUpdateType.AUTOMATIC;
        }

        return result;
    }

    public static void setImportArtFilme(FilmListUpdateType type) {
        final int value;
        if (type == FilmListUpdateType.MANUAL) {
            value = UPDATE_FILME_AUS;
        } else {
            value = UPDATE_FILME_AUTO;
        }

        MVConfig.add(MVConfig.Configs.SYSTEM_IMPORT_ART_FILME, String.valueOf(value));
    }

    public static void enableComponents(Container container, boolean enable) {
        Component[] components = container.getComponents();
        for (Component component : components) {
            component.setEnabled(enable);
            if (component instanceof Container) {
                enableComponents((Container) component, enable);
            }
        }
    }

}
