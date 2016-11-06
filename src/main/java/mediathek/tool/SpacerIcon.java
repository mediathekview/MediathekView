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

import java.awt.Component;
import java.awt.Graphics;
import javax.swing.Icon;
import javax.swing.JTable;

public class SpacerIcon implements Icon {

    private int extraHeight;

    public SpacerIcon(int extraHeight) {
        if (extraHeight < 0) {
            throw new IllegalArgumentException("extraHeight must be >= 0");
        }
        this.extraHeight = extraHeight + new JTable().getFont().getSize();
    }

    @Override
    public int getIconHeight() {
        return extraHeight;
    }

    @Override
    public int getIconWidth() {
        return 0;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
    }
}
