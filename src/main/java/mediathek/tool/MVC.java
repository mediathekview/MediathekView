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
import mediathek.config.MVConfig;

public class MVC {

    public MVConfig.Configs configs = null;
    public String text = "";
    public Color color = new Color(0);
    public Color colorReset = new Color(0);

    public MVC(MVConfig.Configs configs, Color ccolor, String ttext) {
        this.configs = configs;
        text = ttext;
        color = ccolor;
        colorReset = ccolor;
    }

    public void set(Color c) {
        color = c;
    }

    public void reset() {
        color = colorReset;
    }
}
