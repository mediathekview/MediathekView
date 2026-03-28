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

package mediathek.gui.dialogEinstellungen.allgemein;

import com.jidesoft.swing.MultilineLabel;
import mediathek.tool.ApplicationConfiguration;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.configuration2.Configuration;

import javax.swing.*;
import java.awt.*;
import java.util.Arrays;
import java.util.Locale;

public class LuceneDirectoryModePanel extends JPanel {
    private final Configuration config = ApplicationConfiguration.getConfiguration();
    private static final String[] LUCENE_DIRECTORY_MODES = new String[]{"auto", "mmap", "niofs", "in-memory"};
    private final JComboBox<String> modeComboBox = new JComboBox<>(LUCENE_DIRECTORY_MODES);
    private final JLabel descriptionLabel = new JLabel();
    private final JLabel restartWarningIconLabel = new JLabel(UIManager.getIcon("OptionPane.warningIcon"));
    private final JLabel restartWarningTextLabel = new JLabel("<html><b>Neustart notwendig</b></html>");
    private final MultilineLabel infoLabel = new MultilineLabel(
            "Wählen Sie, welche Lucene-Directory-Implementierung verwendet wird.\n" +
            "Die Einstellung beeinflusst Dateiverhalten und Suchperformance."
    );
    private boolean initializing;

    public LuceneDirectoryModePanel() {
        initComponents();
        initializing = true;
        setSelectedMode(config.getString(ApplicationConfiguration.LUCENE_DIRECTORY_MODE, "auto"));
        initializing = false;
        updateDescription();
    }

    public JComboBox<String> getModeComboBox() {
        return modeComboBox;
    }

    public JLabel getDescriptionLabel() {
        return descriptionLabel;
    }

    public String getSelectedMode() {
        var selected = modeComboBox.getSelectedItem();
        return selected == null ? "auto" : selected.toString();
    }

    public void setSelectedMode(String mode) {
        if (mode == null) {
            modeComboBox.setSelectedItem("auto");
            return;
        }

        var normalizedMode = mode.trim().toLowerCase(Locale.ROOT);
        var isKnown = Arrays.asList(LUCENE_DIRECTORY_MODES).contains(normalizedMode);
        modeComboBox.setSelectedItem(isKnown ? normalizedMode : "auto");
    }

    private void initComponents() {
        setLayout(new MigLayout(
                "insets 5,hidemode 3",
                "[right][grow,fill]",
                "[][][][]"));

        add(infoLabel, "cell 0 0 2 1,growx");

        add(new JLabel("Lucene-Directory-Anbieter:"), "cell 0 1");
        add(modeComboBox, "cell 1 1");

        descriptionLabel.setVerticalAlignment(SwingConstants.TOP);
        add(descriptionLabel, "cell 0 2 2 1,growx");
        restartWarningIconLabel.setVisible(false);
        restartWarningTextLabel.setVisible(false);
        var restartWarningPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        restartWarningPanel.setOpaque(false);
        restartWarningPanel.add(restartWarningIconLabel);
        restartWarningPanel.add(restartWarningTextLabel);
        add(restartWarningPanel, "cell 0 3 2 1,growx,alignx left");

        modeComboBox.addActionListener(_ -> {
            var selectedMode = getSelectedMode();
            config.setProperty(ApplicationConfiguration.LUCENE_DIRECTORY_MODE, selectedMode);
            updateDescription();
            if (!initializing) {
                restartWarningIconLabel.setVisible(true);
                restartWarningTextLabel.setVisible(true);
            }
        });
    }

    private void updateDescription() {
        var mode = getSelectedMode();
        var text = switch (mode) {
            case "mmap" -> "mmap: Oft schnell bei Suchzugriffen, kann aber auf einigen Systemen Dateihandling beim Neuaufbau erschweren.";
            case "niofs" -> "niofs: Robustes Datei-I/O ohne Memory Mapping, meist stabiler beim Austausch/Löschen von Indexdateien.";
            case "in-memory" -> "in-memory: Beste Performance, aber höherer Speicherverbrauch.";
            default -> "auto: Lucene wählt automatisch die passende Directory-Implementierung für das Betriebssystem.";
        };
        descriptionLabel.setText("<html>" + text + "</html>");
    }
}
