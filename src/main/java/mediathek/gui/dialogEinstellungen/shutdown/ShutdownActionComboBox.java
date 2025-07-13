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

package mediathek.gui.dialogEinstellungen.shutdown;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.List;

public class ShutdownActionComboBox extends JComboBox<String> {

    private static final String DOMAIN = "org.mediathekview.mv_shutdown_helper";
    private static final String KEY = "shutdownAction";
    private static final List<String> ACTIONS = Arrays.asList("shutdown", "sleep", "restart");
    private static final Logger logger = LogManager.getLogger();

    public ShutdownActionComboBox() {
        super(new DefaultComboBoxModel<>(ACTIONS.toArray(new String[0])));
        setSelectedItem(readCurrentAction());

        addActionListener(_ -> {
            var selected = (String) getSelectedItem();
            if (selected != null) {
                writeAction(selected);
            }
        });
    }

    private String readCurrentAction() {
        try {
            var pb = new ProcessBuilder("defaults", "read", DOMAIN, KEY);
            pb.redirectErrorStream(true);
            var process = pb.start();

            try (var isr = new InputStreamReader(process.getInputStream());
                 var reader = new BufferedReader(isr)) {
                var line = reader.readLine();
                if (line != null && ACTIONS.contains(line.trim().toLowerCase())) {
                    return line.trim().toLowerCase();
                }
            }
        }
        catch (Exception _) {
        }
        return "shutdown";
    }

    private void writeAction(String action) {
        try {
            new ProcessBuilder("defaults", "write", DOMAIN, KEY, "-string", action)
                    .inheritIO()
                    .start()
                    .waitFor();
        }
        catch (Exception e) {
            System.err.println("Konnte defaults-Wert nicht schreiben: " + e.getMessage());
            logger.error("Failed to write shutdown action", e);
        }
    }
}
