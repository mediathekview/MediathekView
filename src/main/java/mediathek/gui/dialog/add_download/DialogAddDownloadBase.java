/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.gui.dialog.add_download;

import mediathek.gui.tabs.tab_film.SenderIconLabel;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXBusyLabel;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;

/**
 * Base class for JFormDesigner.
 * Subclass contains the dialog behavior.
 */
public class DialogAddDownloadBase extends JDialog {
    public DialogAddDownloadBase(Frame parent) {
        super(parent, true);
        initComponents();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var panel1 = new JPanel();
        btnQueueDownload = new JButton();
        btnDownloadImmediately = new JButton();
        jButtonAbbrechen = new JButton();
        var jPanel7 = new JPanel();
        var jLabelSet = new JLabel();
        jComboBoxPset = new javax.swing.JComboBox<>();
        var jLabel1 = new JLabel();
        var jPanel4 = new JPanel();
        jComboBoxPfad = new javax.swing.JComboBox<>();
        jButtonZiel = new JButton();
        jButtonDelHistory = new JButton();
        var jLabel4 = new JLabel();
        jTextFieldName = new JTextField();
        var jPanel2 = new JPanel();
        jCheckBoxInfodatei = new JCheckBox();
        jCheckBoxPfadSpeichern = new JCheckBox();
        jCheckBoxSubtitle = new JCheckBox();
        jCheckBoxMp4Metadata = new JCheckBox();
        jPanelSize = new JPanel();
        var jPanel6 = new JPanel();
        jRadioButtonAufloesungHd = new JRadioButton();
        jRadioButtonAufloesungHoch = new JRadioButton();
        jRadioButtonAufloesungKlein = new JRadioButton();
        var jPanel3 = new JPanel();
        btnRequestLiveInfo = new JButton();
        lblBusyIndicator = new JXBusyLabel();
        var jPanel5 = new JPanel();
        lblStatus = new JLabel();
        lblAudioInfo = new JLabel();
        var filmPanel = new JPanel();
        lblSenderIcon = new SenderIconLabel();
        var filmTextPanel = new JPanel();
        lblFilmTitle = new JLabel();
        lblFilmThema = new JLabel();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Film speichern");
        var contentPane = getContentPane();
        contentPane.setLayout(new MigLayout(
            new LC().fillX().insets("dialog").hideMode(3),
            // columns
            new AC()
                .grow().fill(),
            // rows
            new AC()
                .gap()
                .gap()
                .gap()
                .gap()
                ));

        //======== panel1 ========
        {
            panel1.setLayout(new MigLayout(
                new LC().insets("0").hideMode(3).alignX("right").gridGap("5", "5"),
                // columns
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill()));

            //---- btnQueueDownload ----
            btnQueueDownload.setText("In die Warteschlange");
            panel1.add(btnQueueDownload, new CC().cell(0, 0));

            //---- btnDownloadImmediately ----
            btnDownloadImmediately.setText("Sofort laden");
            panel1.add(btnDownloadImmediately, new CC().cell(1, 0));

            //---- jButtonAbbrechen ----
            jButtonAbbrechen.setText("Abbrechen");
            panel1.add(jButtonAbbrechen, new CC().cell(2, 0));
        }
        contentPane.add(panel1, new CC().cell(0, 4).alignX("right"));

        //======== jPanel7 ========
        {
            jPanel7.setLayout(new MigLayout(
                new LC().fillX().insets("0").hideMode(3).gridGap("5", "8"),
                // columns
                new AC()
                    .align("right").gap()
                    .grow().fill(),
                // rows
                new AC()
                    .gap()
                    .gap()
                    ));

            //---- jLabelSet ----
            jLabelSet.setText("Set:");
            jPanel7.add(jLabelSet, new CC().cell(0, 0));
            jPanel7.add(jComboBoxPset, new CC().cell(1, 0).growX());

            //---- jLabel1 ----
            jLabel1.setText("Zielpfad:");
            jPanel7.add(jLabel1, new CC().cell(0, 1));

            //======== jPanel4 ========
            {
                jPanel4.setLayout(new MigLayout(
                    new LC().fillX().insets("0").hideMode(3).gridGap("5", "0"),
                    // columns
                    new AC()
                        .grow().fill().gap()
                        .size("pref!").gap()
                        .size("pref!"),
                    // rows
                    new AC()
                        ));

                //---- jComboBoxPfad ----
                jComboBoxPfad.setEditable(true);
                jPanel4.add(jComboBoxPfad, new CC().cell(0, 0).pushX().growX());

                //---- jButtonZiel ----
                jButtonZiel.setText("F");
                jButtonZiel.setToolTipText("Zielpfad ausw\u00e4hlen");
                jPanel4.add(jButtonZiel, new CC().cell(1, 0));

                //---- jButtonDelHistory ----
                jButtonDelHistory.setText("H");
                jButtonDelHistory.setToolTipText("History l\u00f6schen");
                jPanel4.add(jButtonDelHistory, new CC().cell(2, 0));
            }
            jPanel7.add(jPanel4, new CC().cell(1, 1).growX());

            //---- jLabel4 ----
            jLabel4.setText("Dateiname:");
            jPanel7.add(jLabel4, new CC().cell(0, 2));

            //---- jTextFieldName ----
            jTextFieldName.setColumns(30);
            jTextFieldName.setMinimumSize(new Dimension(0, 26));
            jPanel7.add(jTextFieldName, new CC().cell(1, 2).pushX().growX());
        }
        contentPane.add(jPanel7, new CC().cell(0, 1).growX());

        //======== jPanel2 ========
        {
            jPanel2.setLayout(new MigLayout(
                new LC().fillX().insets("0").hideMode(3).gridGap("20", "6"),
                // columns
                new AC()
                    .grow().fill().gap()
                    .grow().fill(),
                // rows
                new AC()
                    .gap()
                    .gap()
                    ));

            //---- jCheckBoxInfodatei ----
            jCheckBoxInfodatei.setText("Lege Infodatei an");
            jCheckBoxInfodatei.setToolTipText("Erzeugt eine Infodatei im Format \"Infodatei.txt\"");
            jPanel2.add(jCheckBoxInfodatei, new CC().cell(0, 0).alignX("left"));

            //---- jCheckBoxPfadSpeichern ----
            jCheckBoxPfadSpeichern.setText("Zielpfad speichern");
            jPanel2.add(jCheckBoxPfadSpeichern, new CC().cell(1, 0).alignX("left"));
            jPanel2.add(jCheckBoxSubtitle, new CC().cell(0, 1).alignX("left"));

            //---- jCheckBoxMp4Metadata ----
            jCheckBoxMp4Metadata.setText("MP4-Metadaten schreiben");
            jCheckBoxMp4Metadata.setToolTipText("Schreibt Standard-MP4-Metadaten nach dem Download in die fertige MP4-Datei");
            jPanel2.add(jCheckBoxMp4Metadata, new CC().cell(1, 1).alignX("left"));
        }
        contentPane.add(jPanel2, new CC().cell(0, 2).growX());

        //======== jPanelSize ========
        {
            jPanelSize.setBorder(new TitledBorder("Download-Qualit\u00e4t"));
            jPanelSize.setLayout(new MigLayout(
                new LC().fillX().insets("0").hideMode(3),
                // columns
                new AC()
                    .grow().fill(),
                // rows
                new AC()
                    .gap()
                    ));

            //======== jPanel6 ========
            {
                jPanel6.setLayout(new MigLayout(
                    new LC().insets("0").hideMode(3).alignX("left").gridGap("20", "0"),
                    // columns
                    new AC()
                        .size("pref!").gap()
                        .size("pref!").gap()
                        .size("pref!"),
                    // rows
                    new AC()
                        ));

                //---- jRadioButtonAufloesungHd ----
                jRadioButtonAufloesungHd.setText("H\u00f6chste/Hoch");
                jPanel6.add(jRadioButtonAufloesungHd, new CC().cell(0, 0));

                //---- jRadioButtonAufloesungHoch ----
                jRadioButtonAufloesungHoch.setText("Mittel");
                jPanel6.add(jRadioButtonAufloesungHoch, new CC().cell(1, 0));

                //---- jRadioButtonAufloesungKlein ----
                jRadioButtonAufloesungKlein.setText("Niedrig");
                jPanel6.add(jRadioButtonAufloesungKlein, new CC().cell(2, 0));
            }
            jPanelSize.add(jPanel6, new CC().cell(0, 0).alignX("left"));

            //======== jPanel3 ========
            {
                jPanel3.setLayout(new MigLayout(
                    new LC().fillX().insets("0").hideMode(3).gridGap("10", "0"),
                    // columns
                    new AC()
                        .size("pref!").gap()
                        .size("pref!").gap()
                        .grow().fill(),
                    // rows
                    new AC()
                        ));

                //---- btnRequestLiveInfo ----
                btnRequestLiveInfo.setText("Codec-Details abrufen...");
                jPanel3.add(btnRequestLiveInfo, new CC().cell(0, 0).alignY("top"));
                jPanel3.add(lblBusyIndicator, new CC().cell(1, 0).alignY("top"));

                //======== jPanel5 ========
                {
                    jPanel5.setLayout(new GridLayout(2, 1));

                    //---- lblStatus ----
                    lblStatus.setText("Video: n/a");
                    jPanel5.add(lblStatus);

                    //---- lblAudioInfo ----
                    lblAudioInfo.setText("Audio: n/a");
                    jPanel5.add(lblAudioInfo);
                }
                jPanel3.add(jPanel5, new CC().cell(2, 0).pushX().growX());
            }
            jPanelSize.add(jPanel3, new CC().cell(0, 1).growX());
        }
        contentPane.add(jPanelSize, new CC().cell(0, 3).growX());

        //======== filmPanel ========
        {
            filmPanel.setBorder(BorderFactory.createEtchedBorder());
            filmPanel.setLayout(new MigLayout(
                new LC().fillX().insets("5").hideMode(3).gridGap("8", "0"),
                // columns
                new AC()
                    .size("pref!").gap()
                    .grow().fill(),
                // rows
                new AC()
                    .align("top")));

            //---- lblSenderIcon ----
            lblSenderIcon.setMaximumSize(new Dimension(64, 64));
            lblSenderIcon.setMinimumSize(new Dimension(64, 64));
            lblSenderIcon.setPreferredSize(new Dimension(64, 64));
            filmPanel.add(lblSenderIcon, new CC().cell(0, 0).alignY("top"));

            //======== filmTextPanel ========
            {
                filmTextPanel.setLayout(new MigLayout(
                    new LC().fillX().insets("0").hideMode(3).gridGap("0", "2"),
                    // columns
                    new AC()
                        .grow().fill(),
                    // rows
                    new AC()
                        .gap()
                        ));

                //---- lblFilmTitle ----
                lblFilmTitle.setFont(lblFilmTitle.getFont().deriveFont(lblFilmTitle.getFont().getStyle() | Font.BOLD));
                lblFilmTitle.setMinimumSize(new Dimension(0, 0));
                lblFilmTitle.setText("Tatort, ...");
                filmTextPanel.add(lblFilmTitle, new CC().cell(0, 0).pushX().growX());

                //---- lblFilmThema ----
                lblFilmThema.setMinimumSize(new Dimension(0, 0));
                lblFilmThema.setText("Thema");
                filmTextPanel.add(lblFilmThema, new CC().cell(0, 1).pushX().growX());
            }
            filmPanel.add(filmTextPanel, new CC().cell(1, 0).pushX().growX());
        }
        contentPane.add(filmPanel, new CC().cell(0, 0).growX());
        pack();
        setLocationRelativeTo(getOwner());

        //---- buttonGroup1 ----
        var buttonGroup1 = new ButtonGroup();
        buttonGroup1.add(jRadioButtonAufloesungHd);
        buttonGroup1.add(jRadioButtonAufloesungHoch);
        buttonGroup1.add(jRadioButtonAufloesungKlein);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JButton btnQueueDownload;
    protected JButton btnDownloadImmediately;
    protected JButton jButtonAbbrechen;
    protected JComboBox<String> jComboBoxPset;
    protected JComboBox<String> jComboBoxPfad;
    protected JButton jButtonZiel;
    protected JButton jButtonDelHistory;
    protected JTextField jTextFieldName;
    protected JCheckBox jCheckBoxInfodatei;
    protected JCheckBox jCheckBoxPfadSpeichern;
    protected JCheckBox jCheckBoxSubtitle;
    protected JCheckBox jCheckBoxMp4Metadata;
    protected JPanel jPanelSize;
    protected JRadioButton jRadioButtonAufloesungHd;
    protected JRadioButton jRadioButtonAufloesungHoch;
    protected JRadioButton jRadioButtonAufloesungKlein;
    protected JButton btnRequestLiveInfo;
    protected JXBusyLabel lblBusyIndicator;
    protected JLabel lblStatus;
    protected JLabel lblAudioInfo;
    protected SenderIconLabel lblSenderIcon;
    protected JLabel lblFilmTitle;
    protected JLabel lblFilmThema;
    // End of variables declaration//GEN-END:variables

}
