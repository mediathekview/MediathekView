/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.logging;

import javax.swing.*;
import java.awt.*;

public class LogDialog extends JDialog {
    public LogDialog(JFrame parent) {
        super(parent);
        setDefaultCloseOperation(HIDE_ON_CLOSE);
        setTitle("Programm-Log");
        setLayout(new BorderLayout());
        add(new LogPanel(), BorderLayout.CENTER);
        setSize(800, 400);
    }
}
