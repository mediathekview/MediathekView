package mediathek.tool;

import mediathek.config.MVConfig;
import mediathek.config.MVConfig.Configs;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.intellij.lang.annotations.MagicConstant;

import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.StringSelection;
import java.awt.event.InputEvent;
import java.io.File;

public class GuiFunktionen {

    /**
     * legacy constant, used internally only
     */
    private static final int UPDATE_FILME_AUS = 0;
    /**
     * legacy constant, used internally only
     */
    private static final int UPDATE_FILME_AUTO = 2;

    private static final Logger logger = LogManager.getLogger();

    /**
     * Determine the image's size while keeping aspect ratio within a given boundary box.
     *
     * @param imgSize  The size of the original image.
     * @param boundary The bounds where the image needs to fit into.
     * @return The calculated image dimensions for fitting into boundary.
     */
    public static Dimension calculateFittedDimension(Dimension imgSize, Dimension boundary) {

        int original_width = imgSize.width;
        int original_height = imgSize.height;
        int bound_width = boundary.width;
        int bound_height = boundary.height;
        int new_width = original_width;
        int new_height = original_height;

        // first check if we need to scale width
        if (original_width > bound_width) {
            //scale width to fit
            new_width = bound_width;
            //scale height to maintain aspect ratio
            new_height = (new_width * original_height) / original_width;
        }

        // then check if we need to scale even with the new height
        if (new_height > bound_height) {
            //scale height to fit instead
            new_height = bound_height;
            //scale width to maintain aspect ratio
            new_width = (new_height * original_width) / original_height;
        }

        return new Dimension(new_width, new_height);
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

    @Deprecated
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
            logger.error("addsPfad({},{}):", pfad1, pfad2);
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

    public static String cutName(String name, int length) {
        if (name.length() > length) {
            name = name.substring(0, length - 4) + name.substring(name.length() - 4);
        }
        return name;
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
            logger.error("getDateiName({})", pfad);
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
            logger.error("getSuffixFromUrl({})", pfad);
        }
        if (ret.contains("?")) {
            ret = ret.substring(0, ret.indexOf('?'));
        }
        if (ret.length() > 5) {
            // dann ist was faul
            ret = "---";
            logger.error("getSuffixFromUrl({})", pfad);
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
            logger.error("getFileNameWithoutSuffix({})", pfad);
        }
        return ret;
    }

    /**
     * Maps the "command" key to the correspondig icon based on operating system.
     *
     * @return an InputEvent modifier based on operating system.
     */
    @MagicConstant(flagsFromClass = java.awt.event.InputEvent.class)
    public static int getPlatformControlKey() {
        int result;

        if (SystemUtils.IS_OS_MAC_OSX) {
            result = InputEvent.META_DOWN_MASK;
        } else {
            result = InputEvent.CTRL_DOWN_MASK;
        }

        return result;
    }

    /**
     * Get the the user set filmlist update type.
     *
     * @return MANUAL or AUTOMATIC based on config. Default is AUTOMATIC.
     */
    public static FilmListUpdateType getFilmListUpdateType() {
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

    /**
     * Store filmlist update mode in config.
     *
     * @param type MANUAL or AUTOMATIC mode.
     */
    public static void setFilmListUpdateType(FilmListUpdateType type) {
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
