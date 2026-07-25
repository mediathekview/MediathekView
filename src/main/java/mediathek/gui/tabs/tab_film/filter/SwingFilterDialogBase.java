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
package mediathek.gui.tabs.tab_film.filter;

import com.jidesoft.swing.JideSplitButton;
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBox;
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBoxModel;
import mediathek.swing.CheckBoxList;
import mediathek.swing.StrictSearchComboBox;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.VerticalLayout;
import org.jspecify.annotations.NonNull;

import javax.swing.*;
import java.awt.*;

public class SwingFilterDialogBase extends JDialog {
    protected SwingFilterDialogBase(@NonNull Window owner, @NonNull FilterSelectionComboBoxModel model) {
        super(owner);
        initComponents();
        cboxFilterSelection.setModel(model);
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var pnlFilterCommon = new JPanel();
        cboxFilterSelection = new FilterSelectionComboBox();
        cbLockCurrentFilter = new JCheckBox();
        btnSplit = new JideSplitButton();
        var separator2 = new JSeparator();
        var pnlShowOnly = new JPanel();
        cbShowNewOnly = new JCheckBox();
        cbShowBookMarkedOnly = new JCheckBox();
        cbShowOnlyHq = new JCheckBox();
        cbShowSubtitlesOnly = new JCheckBox();
        cbShowOnlyLivestreams = new JCheckBox();
        var separator3 = new JSeparator();
        var pnlDontShow = new JPanel();
        cbShowUnseenOnly = new JCheckBox();
        cbDontShowAbos = new JCheckBox();
        cbDontShowSignLanguage = new JCheckBox();
        cbDontShowTrailers = new JCheckBox();
        cbDontShowAudioVersions = new JCheckBox();
        cbDontShowDuplicates = new JCheckBox();
        cbDontShowGeoblocked = new JCheckBox();
        var separator4 = new JSeparator();
        var pnlSenderlist = new JPanel();
        label3 = new JLabel();
        scpSenderList = new JScrollPane();
        senderList = new CheckBoxList();
        var separator5 = new JSeparator();
        var pnlThema = new JPanel();
        label4 = new JLabel();
        jcbThema = new StrictSearchComboBox();
        btnResetThema = new JButton();
        var separator6 = new JSeparator();
        var pnlFlimlength = new JPanel();
        label5 = new JLabel();
        lblMinFilmLengthValue = new JLabel();
        var hSpacer1 = new JPanel(null);
        label7 = new JLabel();
        lblMaxFilmLengthValue = new JLabel();
        filmLengthSlider = new FilmLengthSlider();
        var separator7 = new JSeparator();
        var pnlZeitraum = new JPanel();
        label1 = new JLabel();
        spZeitraum = new ZeitraumSpinner();
        label2 = new JLabel();

        //======== this ========
        setType(Window.Type.UTILITY);
        setTitle("Filter");
        var contentPane = getContentPane();
        contentPane.setLayout(new MigLayout(
            new LC().fillX().insets("5").hideMode(3),
            // columns
            new AC()
                .align("left"),
            // rows
            new AC()
                .gap()
                .shrink(0).align("top").gap("0")
                .gap("0")
                .shrink(0).gap("0")
                .gap("0")
                .shrink(0).gap()
                .grow().fill().gap()
                .shrink(0).gap()
                .gap()
                .shrink(0).gap()
                .gap()
                .shrink(0).gap()
                ));

        //======== pnlFilterCommon ========
        {
            pnlFilterCommon.setLayout(new MigLayout(
                new LC().fillX().insets("0").hideMode(3),
                // columns
                new AC()
                    .grow().fill().gap()
                    .fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill()));

            //---- cboxFilterSelection ----
            cboxFilterSelection.setMaximumSize(null);
            cboxFilterSelection.setPreferredSize(null);
            cboxFilterSelection.setMinimumSize(new Dimension(50, 10));
            pnlFilterCommon.add(cboxFilterSelection, new CC().cell(0, 0));

            //---- cbLockCurrentFilter ----
            cbLockCurrentFilter.setText("Sperren");
            cbLockCurrentFilter.setToolTipText("Filteränderungen nur temporär anwenden und nicht speichern");
            pnlFilterCommon.add(cbLockCurrentFilter, new CC().cell(1, 0));

            //======== btnSplit ========
            {
                btnSplit.setAlwaysDropdown(true);
            }
            pnlFilterCommon.add(btnSplit, new CC().cell(2, 0));
        }
        contentPane.add(pnlFilterCommon, new CC().cell(0, 0).growX());
        contentPane.add(separator2, new CC().cell(0, 1).growX());

        //======== pnlShowOnly ========
        {
            pnlShowOnly.setLayout(new VerticalLayout());

            //---- cbShowNewOnly ----
            cbShowNewOnly.setText("Nur neue Filme anzeigen");
            pnlShowOnly.add(cbShowNewOnly);

            //---- cbShowBookMarkedOnly ----
            cbShowBookMarkedOnly.setText("Nur gemerkte Filme anzeigen");
            pnlShowOnly.add(cbShowBookMarkedOnly);

            //---- cbShowOnlyHq ----
            cbShowOnlyHq.setText("Nur High-Quality-Filme (HQ) anzeigen");
            pnlShowOnly.add(cbShowOnlyHq);

            //---- cbShowSubtitlesOnly ----
            cbShowSubtitlesOnly.setText("Nur Filme mit Untertitel anzeigen");
            pnlShowOnly.add(cbShowSubtitlesOnly);

            //---- cbShowOnlyLivestreams ----
            cbShowOnlyLivestreams.setText("Nur Livestreams anzeigen");
            pnlShowOnly.add(cbShowOnlyLivestreams);
        }
        contentPane.add(pnlShowOnly, new CC().cell(0, 2).growX());
        contentPane.add(separator3, new CC().cell(0, 3).growX());

        //======== pnlDontShow ========
        {
            pnlDontShow.setLayout(new VerticalLayout());

            //---- cbShowUnseenOnly ----
            cbShowUnseenOnly.setText("Gesehene Filme nicht anzeigen");
            pnlDontShow.add(cbShowUnseenOnly);

            //---- cbDontShowAbos ----
            cbDontShowAbos.setText("Abos nicht anzeigen");
            pnlDontShow.add(cbDontShowAbos);

            //---- cbDontShowSignLanguage ----
            cbDontShowSignLanguage.setText("Geb\u00e4rdensprache nicht anzeigen");
            pnlDontShow.add(cbDontShowSignLanguage);

            //---- cbDontShowTrailers ----
            cbDontShowTrailers.setText("Trailer/Teaser/Vorschau nicht anzeigen");
            pnlDontShow.add(cbDontShowTrailers);

            //---- cbDontShowAudioVersions ----
            cbDontShowAudioVersions.setText("H\u00f6rfassungen ausblenden");
            pnlDontShow.add(cbDontShowAudioVersions);

            //---- cbDontShowDuplicates ----
            cbDontShowDuplicates.setText("Duplikate nicht anzeigen");
            pnlDontShow.add(cbDontShowDuplicates);

            //---- cbDontShowGeoblocked ----
            cbDontShowGeoblocked.setText("Geo-blockierte Filme nicht anzeigen");
            pnlDontShow.add(cbDontShowGeoblocked);
        }
        contentPane.add(pnlDontShow, new CC().cell(0, 4).growX());
        contentPane.add(separator4, new CC().cell(0, 5).growX());

        //======== pnlSenderlist ========
        {
            pnlSenderlist.setPreferredSize(new Dimension(258, 220));
            pnlSenderlist.setLayout(new MigLayout(
                new LC().fill().insets("0").hideMode(3),
                // columns
                new AC()
                    .align("left"),
                // rows
                new AC()
                    .gap()
                    .grow()));

            //---- label3 ----
            label3.setText("Sender:");
            pnlSenderlist.add(label3, new CC().cell(0, 0));

            //======== scpSenderList ========
            {
                scpSenderList.setPreferredSize(null);
                scpSenderList.setMaximumSize(null);

                //---- senderList ----
                senderList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                senderList.setMaximumSize(null);
                senderList.setMinimumSize(null);
                senderList.setPreferredSize(null);
                scpSenderList.setViewportView(senderList);
            }
            pnlSenderlist.add(scpSenderList, new CC().cell(0, 1).grow().minHeight("50"));
        }
        contentPane.add(pnlSenderlist, new CC().cell(0, 6).growX());
        contentPane.add(separator5, new CC().cell(0, 7).growX());

        //======== pnlThema ========
        {
            pnlThema.setLayout(new MigLayout(
                new LC().fillX().insets("0").hideMode(3),
                // columns
                new AC()
                    .align("left").gap()
                    .grow().fill().gap()
                    .fill(),
                // rows
                new AC()
                    ));

            //---- label4 ----
            label4.setText("Thema:");
            pnlThema.add(label4, new CC().cell(0, 0));

            //---- jcbThema ----
            jcbThema.setMinimumSize(new Dimension(50, 10));
            jcbThema.setPreferredSize(null);
            jcbThema.setMaximumSize(null);
            pnlThema.add(jcbThema, new CC().cell(1, 0).growX());
            pnlThema.add(btnResetThema, new CC().cell(2, 0));
        }
        contentPane.add(pnlThema, new CC().cell(0, 8).growX());
        contentPane.add(separator6, new CC().cell(0, 9).growX());

        //======== pnlFlimlength ========
        {
            pnlFlimlength.setLayout(new MigLayout(
                new LC().fill().insets("0").hideMode(3),
                // columns
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .grow().fill().gap()
                    .fill().gap()
                    .fill(),
                // rows
                new AC()
                    .gap("0")
                    ));

            //---- label5 ----
            label5.setText("Mindestl\u00e4nge:");
            pnlFlimlength.add(label5, new CC().cell(0, 0));

            //---- lblMinFilmLengthValue ----
            lblMinFilmLengthValue.setText("0");
            pnlFlimlength.add(lblMinFilmLengthValue, new CC().cell(1, 0));
            pnlFlimlength.add(hSpacer1, new CC().cell(2, 0).growX());

            //---- label7 ----
            label7.setText("Maximall\u00e4nge:");
            pnlFlimlength.add(label7, new CC().cell(3, 0));

            //---- lblMaxFilmLengthValue ----
            lblMaxFilmLengthValue.setText("100");
            pnlFlimlength.add(lblMaxFilmLengthValue, new CC().cell(4, 0));
            pnlFlimlength.add(filmLengthSlider, new CC().cell(0, 1, 5, 1).growX());
        }
        contentPane.add(pnlFlimlength, new CC().cell(0, 10).growX());
        contentPane.add(separator7, new CC().cell(0, 11).growX());

        //======== pnlZeitraum ========
        {
            pnlZeitraum.setLayout(new MigLayout(
                new LC().fillX().insets("0").hideMode(3),
                // columns
                new AC()
                    .align("left").gap()
                    .grow().fill().gap()
                    .fill(),
                // rows
                new AC()
                    ));

            //---- label1 ----
            label1.setText("Zeitraum:");
            pnlZeitraum.add(label1, new CC().cell(0, 0));
            pnlZeitraum.add(spZeitraum, new CC().cell(1, 0));

            //---- label2 ----
            label2.setText("Tage");
            pnlZeitraum.add(label2, new CC().cell(2, 0));
        }
        contentPane.add(pnlZeitraum, new CC().cell(0, 12).growX());
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    protected FilterSelectionComboBox cboxFilterSelection;
    protected JCheckBox cbLockCurrentFilter;
    protected JideSplitButton btnSplit;
    protected JCheckBox cbShowNewOnly;
    protected JCheckBox cbShowBookMarkedOnly;
    protected JCheckBox cbShowOnlyHq;
    protected JCheckBox cbShowSubtitlesOnly;
    protected JCheckBox cbShowOnlyLivestreams;
    protected JCheckBox cbShowUnseenOnly;
    protected JCheckBox cbDontShowAbos;
    protected JCheckBox cbDontShowSignLanguage;
    protected JCheckBox cbDontShowTrailers;
    protected JCheckBox cbDontShowAudioVersions;
    protected JCheckBox cbDontShowDuplicates;
    protected JCheckBox cbDontShowGeoblocked;
    protected JLabel label3;
    protected JScrollPane scpSenderList;
    protected CheckBoxList senderList;
    protected JLabel label4;
    protected StrictSearchComboBox jcbThema;
    protected JButton btnResetThema;
    protected JLabel label5;
    protected JLabel lblMinFilmLengthValue;
    protected JLabel label7;
    protected JLabel lblMaxFilmLengthValue;
    protected JSlider filmLengthSlider;
    protected JLabel label1;
    protected ZeitraumSpinner spZeitraum;
    protected JLabel label2;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
