/*
 * Copyright (c) 2024-2026 derreisende77.
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

package mediathek.gui.filmInformation;

import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import java.awt.*;

public class FilmInfoFormPanel extends JPanel {
    public FilmInfoFormPanel() {
        initComponents();
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var label1 = new JLabel();
        lblSender = new JLabel();
        var label2 = new JLabel();
        lblThema = new HtmlMultilineLabel();
        var label3 = new JLabel();
        lblTitel = new HtmlMultilineLabel();
        var label4 = new JLabel();
        lblDate = new JLabel();
        var label5 = new JLabel();
        lblUhrzeit = new JLabel();
        var label6 = new JLabel();
        lblDuration = new JLabel();
        var label7 = new JLabel();
        lblSize = new JLabel();
        var label8 = new JLabel();
        cbHq = new DisabledCheckBox();
        var label9 = new JLabel();
        cbSubtitle = new DisabledCheckBox();
        var label12 = new JLabel();
        lblSeason = new JLabel();
        var label14 = new JLabel();
        lblEpisode = new JLabel();
        var label15 = new JLabel();
        lblAvailableUntil = new JLabel();
        var label10 = new JLabel();
        lblGeo = new JLabel();
        var label11 = new JLabel();
        lblAbo = new JLabel();
        hyperlink = new JXHyperlink();
        var label13 = new JLabel();
        descScrollPane = new JScrollPane();
        lblDescription = new JTextPane();

        //======== this ========
        setLayout(new MigLayout(
            new LC().insets("5").hideMode(3),
            // columns
            new AC()
                .fill().gap()
                .grow().fill(),
            // rows
            new AC()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .grow().fill()));

        //---- label1 ----
        label1.setText("Sender:");
        add(label1, new CC().cell(0, 0));
        add(lblSender, new CC().cell(1, 0));

        //---- label2 ----
        label2.setText("Thema:");
        add(label2, new CC().cell(0, 1));
        add(lblThema, new CC().cell(1, 1).growX().minWidth("0"));

        //---- label3 ----
        label3.setText("Titel:");
        add(label3, new CC().cell(0, 2));
        add(lblTitel, new CC().cell(1, 2).growX().minWidth("0"));

        //---- label4 ----
        label4.setText("Datum:");
        add(label4, new CC().cell(0, 3));

        //---- lblDate ----
        lblDate.setText("text");
        add(lblDate, new CC().cell(1, 3));

        //---- label5 ----
        label5.setText("Uhrzeit:");
        add(label5, new CC().cell(0, 4));

        //---- lblUhrzeit ----
        lblUhrzeit.setText("text");
        add(lblUhrzeit, new CC().cell(1, 4));

        //---- label6 ----
        label6.setText("Dauer:");
        add(label6, new CC().cell(0, 5));

        //---- lblDuration ----
        lblDuration.setText("text");
        add(lblDuration, new CC().cell(1, 5));

        //---- label7 ----
        label7.setText("Gr\u00f6\u00dfe (MB):");
        add(label7, new CC().cell(0, 6));

        //---- lblSize ----
        lblSize.setText("text");
        add(lblSize, new CC().cell(1, 6));

        //---- label8 ----
        label8.setText("HQ:");
        add(label8, new CC().cell(0, 7));
        add(cbHq, new CC().cell(1, 7));

        //---- label9 ----
        label9.setText("Untertitel:");
        add(label9, new CC().cell(0, 8));
        add(cbSubtitle, new CC().cell(1, 8));

        //---- label12 ----
        label12.setText("Season:");
        add(label12, new CC().cell(0, 9));

        //---- lblSeason ----
        lblSeason.setText("text");
        add(lblSeason, new CC().cell(1, 9).growX());

        //---- label14 ----
        label14.setText("Episode:");
        add(label14, new CC().cell(0, 10));

        //---- lblEpisode ----
        lblEpisode.setText("text");
        add(lblEpisode, new CC().cell(1, 10).growX());

        //---- label15 ----
        label15.setText("Verf\u00fcgbar bis:");
        add(label15, new CC().cell(0, 11));

        //---- lblAvailableUntil ----
        lblAvailableUntil.setText("text");
        add(lblAvailableUntil, new CC().cell(1, 11));

        //---- label10 ----
        label10.setText("Geo:");
        add(label10, new CC().cell(0, 12));

        //---- lblGeo ----
        lblGeo.setText("text");
        add(lblGeo, new CC().cell(1, 12));

        //---- label11 ----
        label11.setText("Abo:");
        add(label11, new CC().cell(0, 13));

        //---- lblAbo ----
        lblAbo.setText("text");
        add(lblAbo, new CC().cell(1, 13));

        //---- hyperlink ----
        hyperlink.setText("Link zur Webseite");
        add(hyperlink, new CC().cell(0, 14, 2, 1));

        //---- label13 ----
        label13.setText("Beschreibung:");
        add(label13, new CC().cell(0, 15, 2, 1));

        //======== descScrollPane ========
        {

            //---- lblDescription ----
            lblDescription.setMinimumSize(new Dimension(1, 100));
            lblDescription.setPreferredSize(new Dimension(1, 100));
            lblDescription.setMaximumSize(new Dimension(2147483647, 200));
            descScrollPane.setViewportView(lblDescription);
        }
        add(descScrollPane, new CC().cell(0, 16, 2, 1));
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    private JLabel lblSender;
    private HtmlMultilineLabel lblThema;
    private HtmlMultilineLabel lblTitel;
    private JLabel lblDate;
    private JLabel lblUhrzeit;
    private JLabel lblDuration;
    private JLabel lblSize;
    private DisabledCheckBox cbHq;
    private DisabledCheckBox cbSubtitle;
    private JLabel lblSeason;
    private JLabel lblEpisode;
    private JLabel lblAvailableUntil;
    private JLabel lblGeo;
    private JLabel lblAbo;
    private JXHyperlink hyperlink;
    private JScrollPane descScrollPane;
    private JTextPane lblDescription;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on

    JLabel getLblSender() {
        return lblSender;
    }

    HtmlMultilineLabel getLblThema() {
        return lblThema;
    }

    HtmlMultilineLabel getLblTitel() {
        return lblTitel;
    }

    JLabel getLblDate() {
        return lblDate;
    }

    JLabel getLblUhrzeit() {
        return lblUhrzeit;
    }

    JLabel getLblDuration() {
        return lblDuration;
    }

    JLabel getLblSize() {
        return lblSize;
    }

    DisabledCheckBox getCbHq() {
        return cbHq;
    }

    DisabledCheckBox getCbSubtitle() {
        return cbSubtitle;
    }

    JLabel getLblSeason() {
        return lblSeason;
    }

    JLabel getLblEpisode() {
        return lblEpisode;
    }

    JLabel getLblAvailableUntil() {
        return lblAvailableUntil;
    }

    JLabel getLblGeo() {
        return lblGeo;
    }

    JLabel getLblAbo() {
        return lblAbo;
    }

    JXHyperlink getHyperlink() {
        return hyperlink;
    }

    JScrollPane getDescScrollPane() {
        return descScrollPane;
    }

    JTextPane getLblDescription() {
        return lblDescription;
    }
}
