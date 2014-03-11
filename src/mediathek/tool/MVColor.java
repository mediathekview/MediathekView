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
package mediathek.tool;

import java.awt.Color;
import java.util.LinkedList;
import java.util.ListIterator;

public class MVColor {

    public static Color DOWNLOAD_FARBE_ABO = new Color(0, 50, 120);
    public static Color DOWNLOAD_FARBE_ERR = new Color(241, 188, 221);
    // Tabelle Downloads
    public static Color DOWNLOAD_FARBE_WAIT = new Color(239, 244, 255);
    public static Color ABO = new Color(255, 245, 229);
    public static Color ABO_FOREGROUND = new Color(138, 67, 0);
    // Filter wenn RegEx
    public static Color FILTER_REGEX = new Color(153, 214, 255);
    //
    // Farben
    public static MVC FARBE_GRAU = new MVC(new Color(225, 225, 225), "Text", "Text");
    public static MVC FARBE_GRAU_SEL = new MVC(new Color(190, 190, 190), "Text", "text");
    public static Color DOWNLOAD_FARBE_WAIT_SEL = new Color(199, 206, 222);
    // kann bereits angesehen werden
    //    public static Color DOWNLOAD_FARBE_RUN_ANSEHEHN = new Color(241, 216, 140);
    //    public static Color DOWNLOAD_FARBE_RUN_ANSEHEN_SEL = new Color(206, 168, 52);
    // und ist jetzt fertig
    public static Color DOWNLOAD_FARBE_FERTIG = new Color(188, 241, 195);
    public static Color FARBE_FILM_GEOBLOCK_FORGROUND = new Color(255, 0, 30);
    public static Color DOWNLOAD = new Color(229, 239, 255);
    public static Color DOWNLOAD_FOREGROUND = new Color(0, 72, 138);
    public static Color DOWNLOAD_FARBE_FERTIG_SEL = new Color(115, 206, 92);
    public static Color DOWNLOAD_SEL = new Color(127, 178, 255);
    // Tabelle Abos
    public static Color ANSEHEN = new Color(0, 125, 0);
    public static Color FARBE_FILM_GEOBLOCK_BACKGROUND_SEL = new Color(255, 251, 179);
    public static Color DOWNLOAD_FARBE_LIVE = new Color(130, 0, 0);
    public static Color DOWNLOAD_FARBE_ERR_SEL = new Color(206, 92, 128);
    public static Color ABO_SEL = new Color(255, 204, 127);
    // Download läuft
    public static Color DOWNLOAD_FARBE_RUN = new Color(241, 228, 188);
    // Filter wenn RegEx, bei einem Fehler
    public static Color FILTER_REGEX_FEHLER = Color.RED;
    public static Color DOWNLOAD_FARBE_DOWNLOAD = new Color(0, 90, 0);
    public static Color DOWNLOAD_FARBE_RUN_SEL = new Color(206, 178, 92);
    public static Color FARBE_FILM_GEOBLOCK_BACKGROUND = new Color(255, 254, 230);
    public static Color FARBE_FILM_NEU_FORGROUND = new Color(0, 0, 240);

    public LinkedList<MVC> liste = new LinkedList<>();
    public static final int MVC_TEXT = 0;
    public static final int MVC_COLOR = 1;
    public static final int MVC_MAX = 2;

    public MVColor() {
        liste.add(new MVC(DOWNLOAD_FARBE_RUN, "DOWNLOAD_FARBE_RUN", "DOWNLOAD_FARBE_RUN"));
        liste.add(FARBE_GRAU);
        liste.add(FARBE_GRAU_SEL);
    }

    public TModel getModel() {
        Object[] object;
        TModelColor tModel = new TModelColor(new Object[][]{}, new String[]{"Beschreibung", "Farbe"});
        tModel.setRowCount(0);
        ListIterator<MVC> iterator = liste.listIterator();
        while (iterator.hasNext()) {
            MVC mvc = iterator.next();
            object = new Object[MVC_MAX];
            object[MVC_TEXT] = mvc.text;
            object[MVC_COLOR] = mvc;
            tModel.addRow(object);
        }
        return tModel;
    }

}
