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

import javax.swing.*;

public class MVFont {

    public static int fontSize = 12;
    private static int fontNormal = 12;

    public static void initFont() {
        fontNormal = new JTable().getFont().getSize();
        fontSize = getFontSize();
    }

    private static int getFontSize() {
        int msize = fontNormal;
        if (msize < 6) {
            msize = 6;
        }
        return msize;
    }

}
