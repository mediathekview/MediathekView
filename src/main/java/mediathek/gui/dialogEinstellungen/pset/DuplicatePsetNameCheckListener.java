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

package mediathek.gui.dialogEinstellungen.pset;

import mediathek.config.Daten;
import mediathek.daten.DatenPset;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;

public class DuplicatePsetNameCheckListener implements DocumentListener {
    private final JTextField textField;

    public DuplicatePsetNameCheckListener(JTextField textField) {
        this.textField = textField;
    }

    @Override
    public void insertUpdate(DocumentEvent e) {
        duplicateNameCheck();
    }

    @Override
    public void removeUpdate(DocumentEvent e) {
        duplicateNameCheck();
    }

    @Override
    public void changedUpdate(DocumentEvent e) {
        duplicateNameCheck();
    }

    private void markDuplicate() {
        textField.setBackground(Color.ORANGE);
        textField.requestFocusInWindow();
    }

    private void resetDuplicate() {
        textField.setBackground(UIManager.getDefaults().getColor("TextField.background"));
    }

    private void duplicateNameCheck() {
        var count = Daten.listePset.stream()
                .map(DatenPset::getName)
                .filter(name -> name.equals(textField.getText()))
                .count();

        if (count > 1) {
            markDuplicate();
        } else {
            resetDuplicate();
        }
    }
}
