/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
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
package mediathek.config;

import com.formdev.flatlaf.FlatLaf;
import mediathek.tool.MVC;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;

public class MVColor {

    // Tabelle Filme
    public static final MVC FILM_HISTORY = new MVC(MVConfig.Configs.FARBE__FILM_HISTORY, new Color(225, 225, 225), "Filme, gesehen");
    public static final MVC FILM_BOOKMARKED = new MVC(MVConfig.Configs.FARBE__FILM_BOOKMARKED, new Color(204, 238, 255), "Filme, gemerkt");

    // Tabelle Downloads
    public static final MVC DOWNLOAD_IST_ABO = new MVC(MVConfig.Configs.FARBE__DOWNLOAD_IST_ABO, new Color(138, 67, 0), "Download ist ein Abo");
    public static final MVC DOWNLOAD_IST_DIREKTER_DOWNLOAD = new MVC(MVConfig.Configs.FARBE__DOWNLOAD_IST_DIREKTER_DOWNLOAD, new Color(0, 72, 138), "Download ist ein direkter Download");
    public static final MVC DOWNLOAD_ANSEHEN = new MVC(MVConfig.Configs.FARBE__DOWNLOAD_ANSEHEN, new Color(0, 125, 0), "Download kann schon angesehen werden");
    // status Downloads
    public static final MVC DOWNLOAD_WAIT = new MVC(MVConfig.Configs.FARBE__DOWNLOAD_WAIT, new Color(239, 244, 255), "Download, noch nicht gestartet");
    public static final MVC DOWNLOAD_WAIT_SEL = new MVC(MVConfig.Configs.FARBE__DOWNLOAD_WAIT_SEL, new Color(199, 206, 222), "Download, noch nicht gestartet, selektiert");
    public static final MVC DOWNLOAD_RUN = new MVC(MVConfig.Configs.FARBE__DOWNLOAD_RUN, new Color(241, 228, 188), "Download, läuft");
    public static final MVC DOWNLOAD_RUN_SEL = new MVC(MVConfig.Configs.FARBE__DOWNLOAD_RUN_SEL, new Color(206, 178, 92), "Download, läuft, selektiert");
    public static final MVC DOWNLOAD_FERTIG = new MVC(MVConfig.Configs.FARBE__DOWNLOAD_FERTIG, new Color(188, 241, 195), "Download, fertig");
    public static final MVC DOWNLOAD_FERTIG_SEL = new MVC(MVConfig.Configs.FARBE__DOWNLOAD_FERTIG_SEL, new Color(115, 206, 92), "Download, fertig, selektiert");
    public static final MVC DOWNLOAD_FEHLER = new MVC(MVConfig.Configs.FARBE__DOWNLOAD_FEHLER, new Color(241, 188, 221), "Download, fehlerhaft");
    public static final MVC DOWNLOAD_FEHLER_SEL = new MVC(MVConfig.Configs.FARBE__DOWNLOAD_FEHLER_SEL, new Color(206, 92, 128), "Download, fehlerhaft, selektiert");

    // ProgrammGui
    // DialogDownload
    public static final MVC DOWNLOAD_DATEINAME_NEU = new MVC(MVConfig.Configs.FARBE__DOWNLOAD_DATEINAME_NEU, new Color(0, 140, 0), "Download, Dateiname ist neu");
    public static final MVC DOWNLOAD_DATEINAME_ALT = new MVC(MVConfig.Configs.FARBE__DOWNLOAD_DATEINAME_ALT, new Color(0, 0, 200), "Download, Dateiname ist der alte");
    public static final int MVC_TEXT = 0;
    public static final int MVC_COLOR = 1;
    public static final int MVC_MAX = 2;
    private static final Color JTABLE_ALTERNATE_ROW_COLOR = new Color(247, 247, 247);
    public ArrayList<MVC> liste = new ArrayList<>();

    public MVColor() {
        liste.add(FILM_HISTORY);
        liste.add(FILM_BOOKMARKED);
        liste.add(DOWNLOAD_IST_ABO);
        liste.add(DOWNLOAD_IST_DIREKTER_DOWNLOAD);
        liste.add(DOWNLOAD_ANSEHEN);
        liste.add(DOWNLOAD_WAIT);
        liste.add(DOWNLOAD_WAIT_SEL);
        liste.add(DOWNLOAD_RUN);
        liste.add(DOWNLOAD_RUN_SEL);
        liste.add(DOWNLOAD_FERTIG);
        liste.add(DOWNLOAD_FERTIG_SEL);
        liste.add(DOWNLOAD_FEHLER);
        liste.add(DOWNLOAD_FEHLER_SEL);
        liste.add(DOWNLOAD_DATEINAME_NEU);
        liste.add(DOWNLOAD_DATEINAME_ALT);
    }

    /**
     * Get the pattern text color based on L&F dark mode.
     *
     * @return adjusted color for current L&F
     */
    public static Color getRegExPatternColor() {
        Color color;
        if (FlatLaf.isLafDark()) {
            color = UIManager.getColor("Hyperlink.linkColor");
        } else
            color = Color.BLUE;

        return color;
    }

    public static Color getNewColor() {
        return getRegExPatternColor();
    }

    /**
     * Return the alternating row color based on L&F setting.
     *
     * @return alternating color for dark or light L&Fs.
     */
    public static Color getAlternatingRowColor() {
        Color color;

        if (!FlatLaf.isLafDark()) {
            return JTABLE_ALTERNATE_ROW_COLOR;
        } else {
            var tableBg = UIManager.getColor("Table.background");
            color = brightenColor(tableBg, 0.25f);
        }
        return color;
    }

    /**
     * Calculate a brighter color by factor based on HSB values.
     *
     * @param originalColor the original color.
     * @param factor        the factor to brighten.
     * @return the new brighter color.
     */
    private static @NotNull Color brightenColor(@NotNull Color originalColor, float factor) {
        float[] hsb = Color.RGBtoHSB(originalColor.getRed(), originalColor.getGreen(), originalColor.getBlue(), null);
        return Color.getHSBColor(hsb[0], hsb[1], factor * (1f + hsb[2]));
    }

    public final void load() {
        liste.stream().filter(mvc -> !MVConfig.get(mvc.configs).isEmpty()).forEach(mvc -> {
            try {
                mvc.color = new Color(Integer.parseInt(MVConfig.get(mvc.configs)));
            } catch (Exception ignored) {
            }
        });
    }

    public final void save() {
        for (MVC mvc : liste) {
            MVConfig.add(mvc.configs, String.valueOf(mvc.color.getRGB()));
        }
    }

    public void reset() {
        liste.forEach(MVC::reset);
    }
}
