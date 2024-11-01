/*
 * Copyright (c) 2024 derreisende77.
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

package mediathek.gui.dialog.subripmerge;

import com.github.kokorin.jaffree.ffmpeg.FFmpeg;
import com.github.kokorin.jaffree.ffmpeg.UrlInput;
import com.github.kokorin.jaffree.ffmpeg.UrlOutput;
import mediathek.config.Konstanten;
import mediathek.tool.FileDialogs;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.LanguageCode;
import mediathek.tool.SwingErrorDialog;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jdesktop.swingx.JXBusyLabel;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author christianfranzke
 */
public class MergeSubripVideoDialog extends JDialog {
    private static final Logger logger = LogManager.getLogger();
    private static final Pattern PATTERN = Pattern.compile("\\[(.*?)]");

    public MergeSubripVideoDialog(Window owner) {
        super(owner);
        initComponents();

        getRootPane().setDefaultButton(btnMerge);
        busyLabel.setVisible(false);
        btnMerge.setEnabled(false);

        fillLanguageComboBox();
        cbLanguage.setSelectedItem(getLanguageText(LanguageCode.de));

        btnCancel.addActionListener(_ -> dispose());

        setupTextFieldListener();

        btnSelectInputSubrip.addActionListener(_ -> {
            var file = FileDialogs.chooseLoadFileLocation(this, "Untertitel wählen", "");
            if (file != null) {
                var fileStr = file.getAbsolutePath();
                if (!fileStr.toLowerCase().endsWith(".srt")) {
                    JOptionPane.showMessageDialog(this, "Untertiteldatei muss auf .srt enden.", Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
                    tfSubripFilePath.setText("");
                }
                else {
                    tfSubripFilePath.setText(file.getAbsolutePath());
                }
            }
        });
        btnSelectInputVideo.addActionListener(_ -> {
            var file = FileDialogs.chooseLoadFileLocation(this, "Video wählen", "");
            if (file != null) {
                tfVideoFilePath.setText(file.getAbsolutePath());
            }
            else {
                tfVideoFilePath.setText("");
            }
        });

        btnSelectVideoOutputPath.addActionListener(_ -> {
            var file = FileDialogs.chooseSaveFileLocation(this, "Videospeicherort wählen", "");
            if (file != null) {
                tfVideoOutputPath.setText(file.getAbsolutePath());
            }
            else {
                tfVideoOutputPath.setText("");
            }
        });

        btnMerge.addActionListener(_ -> {
            try {
                var lang = (String) cbLanguage.getSelectedItem();
                if (lang == null)
                    throw new IllegalArgumentException("Native language selected is null");

                Matcher matcher = PATTERN.matcher(lang);

                if (matcher.find()) {
                    lang = matcher.group(1);
                } else {
                    throw new IllegalArgumentException("Could not get ISO 639 3 letter code");
                }

                busyLabel.setVisible(true);
                busyLabel.setBusy(true);
                btnMerge.setEnabled(false);
                btnCancel.setEnabled(false);

                var ffmpegPath = GuiFunktionenProgramme.findExecutableOnPath("ffmpeg").getParent();
                var ffmpeg = FFmpeg.atPath(ffmpegPath)
                        .setOverwriteOutput(true)
                        .addArgument("-xerror")
                        .addInput(UrlInput.fromUrl(tfVideoFilePath.getText()))
                        .addInput(UrlInput.fromUrl(tfSubripFilePath.getText()))
                        .addOutput(UrlOutput.toUrl(tfVideoOutputPath.getText()))
                        .addArguments("-c", "copy")
                        .addArguments("-c:s", "mov_text")
                        .addArgument("-metadata:s:s:0")
                        .addArgument("language=" + lang);
                ffmpeg.executeAsync().toCompletableFuture()
                        .thenAccept(_ -> SwingUtilities.invokeLater(() -> {
                            shutdownMergeProcess();
                            JOptionPane.showMessageDialog(MergeSubripVideoDialog.this, "Das Zusammenführen war erfolgreich", Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE);
                            dispose();
                        }))
                        .exceptionally(ex -> {
                            SwingUtilities.invokeLater(() -> {
                                shutdownMergeProcess();
                                SwingErrorDialog.showExceptionMessage(MergeSubripVideoDialog.this, "Der Vorgang war fehlerhaft", ex);
                                dispose();
                            });
                            return null;
                        });
            } catch (Exception e) {
                logger.error("Error occured while merging video", e);
                shutdownMergeProcess();
                SwingErrorDialog.showExceptionMessage(this, "Es ist ein Fehler aufgetreten.", e);
            }
        });
    }

    private void setupTextFieldListener() {
        DocumentListener documentListener = new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                updateButtonState();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                updateButtonState();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                updateButtonState();
            }

            private void updateButtonState() {
                boolean allFieldsFilled = !tfSubripFilePath.getText().trim().isEmpty() &&
                        !tfVideoFilePath.getText().trim().isEmpty() &&
                        !tfVideoOutputPath.getText().trim().isEmpty();
                btnMerge.setEnabled(allFieldsFilled);
            }
        };

        tfSubripFilePath.getDocument().addDocumentListener(documentListener);
        tfVideoFilePath.getDocument().addDocumentListener(documentListener);
        tfVideoOutputPath.getDocument().addDocumentListener(documentListener);
    }

    private void shutdownMergeProcess() {
        busyLabel.setBusy(false);
        busyLabel.setVisible(false);
        btnCancel.setEnabled(true);
    }

    private String getLanguageText(LanguageCode code) {
        return String.format("%s [%s]", code.nativeName(), code.getISO3Language());
    }

    public void fillLanguageComboBox() {
        List<String> languages = new ArrayList<>();
        for (var item : LanguageCode.values()) {
            languages.add(getLanguageText(item));
        }
        cbLanguage.setModel(new DefaultComboBoxModel<>(languages.toArray(new String[0])));
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var dialogPane = new JPanel();
        var contentPanel = new JPanel();
        var label1 = new JLabel();
        tfSubripFilePath = new JTextField();
        btnSelectInputSubrip = new JButton();
        var label2 = new JLabel();
        tfVideoFilePath = new JTextField();
        btnSelectInputVideo = new JButton();
        var label3 = new JLabel();
        cbLanguage = new JComboBox<>();
        var label4 = new JLabel();
        tfVideoOutputPath = new JTextField();
        btnSelectVideoOutputPath = new JButton();
        busyLabel = new JXBusyLabel();
        var buttonBar = new JPanel();
        btnCancel = new JButton();
        btnMerge = new JButton();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setModal(true);
        setTitle("Untertitel zu Video hinzuf\u00fcgen"); //NON-NLS
        var contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());

        //======== dialogPane ========
        {
            dialogPane.setBorder(new EmptyBorder(12, 12, 12, 12));
            dialogPane.setLayout(new BorderLayout());

            //======== contentPanel ========
            {

                //---- label1 ----
                label1.setText("Untertitel-Datei:"); //NON-NLS
                label1.setHorizontalAlignment(SwingConstants.RIGHT);

                //---- tfSubripFilePath ----
                tfSubripFilePath.setToolTipText("Pfad zur Untertiteldatei im Subrip Text Format (.srt)"); //NON-NLS

                //---- btnSelectInputSubrip ----
                btnSelectInputSubrip.setText("..."); //NON-NLS

                //---- label2 ----
                label2.setText("Video-Datei:"); //NON-NLS
                label2.setHorizontalAlignment(SwingConstants.RIGHT);

                //---- tfVideoFilePath ----
                tfVideoFilePath.setToolTipText("Pfad zu einer von ffmpeg unterst\u00fctzten Videodatei als Eingabemedium"); //NON-NLS

                //---- btnSelectInputVideo ----
                btnSelectInputVideo.setText("..."); //NON-NLS

                //---- label3 ----
                label3.setText("Sprache:"); //NON-NLS
                label3.setHorizontalAlignment(SwingConstants.RIGHT);

                //---- cbLanguage ----
                cbLanguage.setToolTipText("Sprache der Untertitel"); //NON-NLS

                //---- label4 ----
                label4.setText("Zieldatei:"); //NON-NLS
                label4.setHorizontalAlignment(SwingConstants.RIGHT);

                //---- btnSelectVideoOutputPath ----
                btnSelectVideoOutputPath.setText("..."); //NON-NLS

                //---- busyLabel ----
                busyLabel.setText("F\u00fchre Video und Untertitel zusammen"); //NON-NLS

                GroupLayout contentPanelLayout = new GroupLayout(contentPanel);
                contentPanel.setLayout(contentPanelLayout);
                contentPanelLayout.setHorizontalGroup(
                    contentPanelLayout.createParallelGroup()
                        .addGroup(contentPanelLayout.createSequentialGroup()
                            .addContainerGap()
                            .addGroup(contentPanelLayout.createParallelGroup()
                                .addGroup(contentPanelLayout.createSequentialGroup()
                                    .addGroup(contentPanelLayout.createParallelGroup(GroupLayout.Alignment.TRAILING, false)
                                        .addComponent(label2, GroupLayout.DEFAULT_SIZE, 104, Short.MAX_VALUE)
                                        .addComponent(label3, GroupLayout.PREFERRED_SIZE, 98, GroupLayout.PREFERRED_SIZE)
                                        .addComponent(label1, GroupLayout.DEFAULT_SIZE, 104, Short.MAX_VALUE))
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addGroup(contentPanelLayout.createParallelGroup()
                                        .addGroup(contentPanelLayout.createSequentialGroup()
                                            .addComponent(tfVideoFilePath)
                                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                            .addComponent(btnSelectInputVideo, GroupLayout.PREFERRED_SIZE, 33, GroupLayout.PREFERRED_SIZE))
                                        .addGroup(contentPanelLayout.createSequentialGroup()
                                            .addComponent(tfSubripFilePath)
                                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                            .addComponent(btnSelectInputSubrip, GroupLayout.PREFERRED_SIZE, 33, GroupLayout.PREFERRED_SIZE))
                                        .addGroup(contentPanelLayout.createSequentialGroup()
                                            .addComponent(cbLanguage, GroupLayout.PREFERRED_SIZE, 155, GroupLayout.PREFERRED_SIZE)
                                            .addGap(0, 0, Short.MAX_VALUE))))
                                .addGroup(GroupLayout.Alignment.TRAILING, contentPanelLayout.createSequentialGroup()
                                    .addGroup(contentPanelLayout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                                        .addGroup(contentPanelLayout.createSequentialGroup()
                                            .addComponent(label4, GroupLayout.PREFERRED_SIZE, 104, GroupLayout.PREFERRED_SIZE)
                                            .addGap(6, 6, 6)
                                            .addComponent(tfVideoOutputPath, GroupLayout.PREFERRED_SIZE, 368, GroupLayout.PREFERRED_SIZE))
                                        .addGroup(GroupLayout.Alignment.LEADING, contentPanelLayout.createSequentialGroup()
                                            .addGap(20, 20, 20)
                                            .addComponent(busyLabel, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)))
                                    .addGap(6, 6, 6)
                                    .addComponent(btnSelectVideoOutputPath, GroupLayout.PREFERRED_SIZE, 33, GroupLayout.PREFERRED_SIZE)))
                            .addContainerGap())
                );
                contentPanelLayout.setVerticalGroup(
                    contentPanelLayout.createParallelGroup()
                        .addGroup(contentPanelLayout.createSequentialGroup()
                            .addContainerGap()
                            .addGroup(contentPanelLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                .addComponent(btnSelectInputSubrip)
                                .addComponent(tfSubripFilePath, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addComponent(label1))
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addGroup(contentPanelLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                .addComponent(label3)
                                .addComponent(cbLanguage, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addGroup(contentPanelLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                .addComponent(btnSelectInputVideo)
                                .addComponent(label2)
                                .addComponent(tfVideoFilePath, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                            .addGap(18, 18, 18)
                            .addGroup(contentPanelLayout.createParallelGroup()
                                .addGroup(contentPanelLayout.createSequentialGroup()
                                    .addGap(7, 7, 7)
                                    .addComponent(label4))
                                .addComponent(tfVideoOutputPath, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addComponent(btnSelectVideoOutputPath))
                            .addGap(18, 18, 18)
                            .addComponent(busyLabel, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                            .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                );
            }
            dialogPane.add(contentPanel, BorderLayout.CENTER);

            //======== buttonBar ========
            {
                buttonBar.setBorder(new EmptyBorder(12, 0, 0, 0));
                buttonBar.setLayout(new GridBagLayout());
                ((GridBagLayout)buttonBar.getLayout()).columnWidths = new int[] {0, 0, 80};
                ((GridBagLayout)buttonBar.getLayout()).columnWeights = new double[] {1.0, 0.0, 0.0};

                //---- btnCancel ----
                btnCancel.setText("Abbrechen"); //NON-NLS
                buttonBar.add(btnCancel, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0,
                    GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                    new Insets(0, 0, 0, 5), 0, 0));

                //---- btnMerge ----
                btnMerge.setText("Zusammenf\u00fchren"); //NON-NLS
                buttonBar.add(btnMerge, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0,
                    GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                    new Insets(0, 0, 0, 0), 0, 0));
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
    private JTextField tfSubripFilePath;
    private JButton btnSelectInputSubrip;
    private JTextField tfVideoFilePath;
    private JButton btnSelectInputVideo;
    private JComboBox<String> cbLanguage;
    private JTextField tfVideoOutputPath;
    private JButton btnSelectVideoOutputPath;
    private JXBusyLabel busyLabel;
    private JButton btnCancel;
    private JButton btnMerge;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
