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

import javax.swing.JTable;
import mSearch.tool.Listener;
import mediathek.config.MVConfig;

public class MVFont {

    public static int fontSize = 12;
    private static int fontNormal = 12;

    public static void initFont() {
        int f;
        fontNormal = new JTable().getFont().getSize();
        try {
            f = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_FONT_SIZE));
        } catch (Exception ignore) {
            f = 0;
            MVConfig.add(MVConfig.Configs.SYSTEM_FONT_SIZE, "0");
        }
        fontSize = getFontSize(f);
    }

    private static int getFontSize(int size) {
        size = fontNormal + 4 * size;
        if (size < 6) {
            size = 6;
        }
        return size;
    }

    public static void resetFontSize() {
        MVConfig.add(MVConfig.Configs.SYSTEM_FONT_SIZE, "0");
        MVFont.fontSize = getFontSize(0);
        Listener.notify(Listener.EREIGNIS_FONT, GuiFunktionen.class.getSimpleName());
    }

    public static void setFontSize(boolean up) {
        int size;
        try {
            size = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_FONT_SIZE));
        } catch (Exception ex) {
            size = 0;
        }
        if (up && size < 10) {
            ++size;
        } else if (!up && size > -5) {
            --size;
            if (MVFont.fontSize == getFontSize(size)) {
                // dann gehts nicht mehr kleiner
                ++size;
            }
        }
        MVConfig.add(MVConfig.Configs.SYSTEM_FONT_SIZE, String.valueOf(size));
        MVFont.fontSize = getFontSize(size);
        Listener.notify(Listener.EREIGNIS_FONT, GuiFunktionen.class.getSimpleName());
    }

}
