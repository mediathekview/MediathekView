/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.swing;

import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXBusyLabel;

import javax.swing.*;

/**
 * This will display a JPanel with a indefinite progress indicator and some status
 * messages used as a glass pane overlay during app termination.
 */
public class AppTerminationIndefiniteProgress extends JPanel {
    private final JLabel lblMessage = new JLabel("Warte auf Abschluss der Downloads...");

    public AppTerminationIndefiniteProgress(boolean willbeShutDown) {
        super();

        setLayout(new MigLayout(new LC().hideMode(3),
                new AC().fill().fill(),new AC()));

        var busyLabel = new JXBusyLabel();
        add(busyLabel, new CC().cell(0,0).span(1,3));
        busyLabel.setBusy(true);

        add(lblMessage, new CC().cell(1,0));
        if (willbeShutDown) {
            add(new JLabel("Der Rechner wird danach heruntergefahren."), new CC().cell(1,1));
        }
        add(new JLabel("Sie können den Vorgang mit Escape abbrechen."), new CC().cell(1,2));
    }

    public void setMessage(String text) {
        SwingUtilities.invokeLater(() -> lblMessage.setText(text));
    }
}
