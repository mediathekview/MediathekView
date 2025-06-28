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

/*
 * Created by JFormDesigner on Sat Jun 28 13:51:34 CEST 2025
 */

package mediathek.gui.bookmark.dialog;

import mediathek.gui.bookmark.BookmarkData;
import mediathek.swing.IconUtils;
import mediathek.tool.EscapeKeyHandler;
import mediathek.tool.datum.DateUtil;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXDatePicker;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.kordamp.ikonli.materialdesign2.MaterialDesignE;

import javax.swing.*;
import java.awt.*;
import java.time.LocalDate;

public class BookmarkEditNoteDialog extends JDialog {
    private boolean okPressed;
    private final BookmarkData bm;

    public BookmarkEditNoteDialog(Dialog owner, @NotNull BookmarkData bm) {
        super(owner);
        this.bm = bm;

        initComponents();
        EscapeKeyHandler.installHandler(this, this::dispose);

        setupButtonBar();
        setupNotizArea();
        setupDatePicker();

        btnSearch.setIcon(IconUtils.of(MaterialDesignE.EYE, 20));
        btnSearch.addActionListener(_ -> {
            JOptionPane.showMessageDialog(this,"Noch nicht implementiert, kommt noch :)");
        });

        textArea.requestFocus();
    }

    private void setupButtonBar() {
        okButton.addActionListener(_ -> {
            okPressed = true;
            dispose();
        });
        cancelButton.addActionListener(_ -> dispose());
    }

    private void setupNotizArea() {
        var initialText = bm.getNote();
        textArea.setText(initialText != null ? initialText : "");
        textArea.setCaretPosition(0);
    }

    private void setupDatePicker() {
        var localAvailableUntilDate = bm.getAvailableUntil();
        if (localAvailableUntilDate != null) {
            datePicker.setDate(DateUtil.convertToDate(localAvailableUntilDate));
        }
    }

    public String getNotiz() {
        return textArea.getText();
    }

    public @Nullable LocalDate getAvailableUntilDate() {
        return DateUtil.convertToLocalDate(datePicker.getDate());
    }

    public boolean isOkPressed() {
        return okPressed;
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var dialogPane = new JPanel();
        var contentPanel = new JPanel();
        var label1 = new JLabel();
        datePicker = new JXDatePicker();
        btnSearch = new JButton();
        var label2 = new JLabel();
        var scrollPane1 = new JScrollPane();
        textArea = new JTextArea();
        var buttonBar = new JPanel();
        okButton = new JButton();
        cancelButton = new JButton();

        //======== this ========
        setTitle("Neue Anmerkungen");
        setModal(true);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setPreferredSize(null);
        setMinimumSize(null);
        setMaximumSize(null);
        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());

        //======== dialogPane ========
        {
            dialogPane.setLayout(new BorderLayout());

            //======== contentPanel ========
            {
                contentPanel.setLayout(new MigLayout(
                    new LC().insets("dialog").hideMode(3),
                    // columns
                    new AC()
                        .fill().gap()
                        .fill().gap()
                        .fill().gap()
                        .grow().fill(),
                    // rows
                    new AC()
                        .gap()
                        .grow().align("top")));

                //---- label1 ----
                label1.setText("Verf\u00fcgbar bis:");
                contentPanel.add(label1, new CC().cell(0, 0));
                contentPanel.add(datePicker, new CC().cell(1, 0));
                contentPanel.add(btnSearch, new CC().cell(2, 0));

                //---- label2 ----
                label2.setText("Notiz:");
                contentPanel.add(label2, new CC().cell(0, 1));

                //======== scrollPane1 ========
                {
                    scrollPane1.setMinimumSize(new Dimension(400, 150));
                    scrollPane1.setPreferredSize(new Dimension(400, 150));

                    //---- textArea ----
                    textArea.setLineWrap(true);
                    textArea.setWrapStyleWord(true);
                    textArea.setMinimumSize(null);
                    textArea.setPreferredSize(null);
                    scrollPane1.setViewportView(textArea);
                }
                contentPanel.add(scrollPane1, new CC().cell(1, 1, 3, 1).grow());
            }
            dialogPane.add(contentPanel, BorderLayout.CENTER);

            //======== buttonBar ========
            {
                buttonBar.setLayout(new MigLayout(
                    new LC().insets("dialog").alignX("right"),
                    // columns
                    new AC()
                        .size("button").fill().gap()
                        .size("button").fill(),
                    // rows
                    new AC()
                        ));

                //---- okButton ----
                okButton.setText("OK");
                buttonBar.add(okButton, new CC().cell(0, 0));

                //---- cancelButton ----
                cancelButton.setText("Abbrechen");
                buttonBar.add(cancelButton, new CC().cell(1, 0));
            }
            dialogPane.add(buttonBar, BorderLayout.SOUTH);
        }
        contentPane.add(dialogPane, BorderLayout.CENTER);
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    private JXDatePicker datePicker;
    private JButton btnSearch;
    private JTextArea textArea;
    private JButton okButton;
    private JButton cancelButton;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
