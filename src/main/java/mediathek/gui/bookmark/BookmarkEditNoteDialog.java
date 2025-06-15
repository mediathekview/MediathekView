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

package mediathek.gui.bookmark;

import mediathek.tool.EscapeKeyHandler;

import javax.swing.*;
import java.awt.*;

public class BookmarkEditNoteDialog extends JDialog {
    private final JTextArea textArea;
    private boolean okPressed = false;

    public BookmarkEditNoteDialog(Dialog owner, String initialText) {
        super(owner, "Notiz eingeben", true);
        setLayout(new BorderLayout(10, 10));
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        EscapeKeyHandler.installHandler(this, this::dispose);

        textArea = new JTextArea(10, 40);
        textArea.setLineWrap(true);
        textArea.setWrapStyleWord(true);
        textArea.setText(initialText != null ? initialText : "");
        JScrollPane scrollPane = new JScrollPane(textArea);

        JButton okButton = new JButton("OK");
        JButton cancelButton = new JButton("Abbrechen");

        okButton.addActionListener(_ -> {
            okPressed = true;
            dispose();
        });

        cancelButton.addActionListener(_ -> dispose());

        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        buttonPanel.add(okButton);
        buttonPanel.add(cancelButton);

        add(scrollPane, BorderLayout.CENTER);
        add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setLocationRelativeTo(owner);
    }

    public String getNotiz() {
        return textArea.getText();
    }

    public boolean isOkPressed() {
        return okPressed;
    }
}
