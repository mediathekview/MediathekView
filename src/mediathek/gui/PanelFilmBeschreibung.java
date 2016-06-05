/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui;

import java.net.URISyntaxException;
import javax.swing.JFrame;
import javax.swing.JPanel;
import mediathek.daten.Daten;
import mediathek.gui.dialog.DialogFilmBeschreibung;
import mediathek.res.GetIcon;
import mediathek.tool.BeobMausUrl;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import mediathek.tool.MVFont;
import mediathek.tool.UrlHyperlinkAction;
import msearch.daten.DatenFilm;

/**
 *
 * @author emil
 */
public class PanelFilmBeschreibung extends JPanel {

    Daten daten;
    DatenFilm aktFilm = null;
    JFrame parent;

    public PanelFilmBeschreibung(JFrame pparent, Daten dd) {
        initComponents();
        parent = pparent;
        daten = dd;
        jCheckBoxBeschreibung.setIcon(GetIcon.getProgramIcon("close_15.png"));
        jCheckBoxBeschreibung.addActionListener(e -> {
            Daten.mVConfig.add(MVConfig.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN, Boolean.FALSE.toString());
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PANEL_BESCHREIBUNG_ANZEIGEN, PanelFilmBeschreibung.class.getSimpleName());
        });
        jXHyperlinkWebsite.setText("");
        try {
            jXHyperlinkWebsite.setAction(new UrlHyperlinkAction(parent, ""));
        } catch (URISyntaxException ignored) {
        }
        jXHyperlinkWebsite.addMouseListener(new BeobMausUrl(jXHyperlinkWebsite));
        jEditorPane.setEditable(false);
        jEditorPane.setContentType("text/html");
        jCheckBoxChange.setIcon(GetIcon.getProgramIcon("edit_16.png"));
        jCheckBoxChange.addActionListener(e -> {
            if (aktFilm != null) {
                String akt = aktFilm.arr[DatenFilm.FILM_BESCHREIBUNG_NR];
                new DialogFilmBeschreibung(parent, daten, aktFilm).setVisible(true);
                if (!aktFilm.arr[DatenFilm.FILM_BESCHREIBUNG_NR].equals(akt)) {
                    // dann hat sich die Beschreibung geändert
                    setText();
                    Daten.filmlisteSpeichern();
                    ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BESCHREIBUNG, PanelFilmBeschreibung.class.getSimpleName());
                }
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_FONT, PanelFilmBeschreibung.class.getSimpleName()) {
            @Override
            public void ping() {
                setText();
            }
        });
    }

    public void setAktFilm(DatenFilm aaktFilm) {
        aktFilm = aaktFilm;
        setText();
    }

    public void setText() {
        if (aktFilm == null) {
            jEditorPane.setText("");
            jXHyperlinkWebsite.setText("");
        } else {
            // Beschreibung setzen
            jEditorPane.setText(
                    "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
                    + "<head><style type=\"text/css\">.sans { font-family: Verdana, Geneva, sans-serif; font-size: " + MVFont.fontSize + "pt; }</style></head>\n"
                    + "<body>"
                    + "<span class=\"sans\"><b>" + (aktFilm.arr[DatenFilm.FILM_SENDER_NR].isEmpty() ? "" : aktFilm.arr[DatenFilm.FILM_SENDER_NR] + "  -  ")
                    + aktFilm.arr[DatenFilm.FILM_TITEL_NR] + "</b><br /></span>"
                    + "<span class=\"sans\">" + aktFilm.arr[DatenFilm.FILM_BESCHREIBUNG_NR].replace("\n", "<br />") + "</span>"
                    + "</body>"
                    + "</html>");

            jXHyperlinkWebsite.setText(aktFilm.arr[DatenFilm.FILM_WEBSEITE_NR]);
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jCheckBoxBeschreibung = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        javax.swing.JScrollPane jScrollPane2 = new javax.swing.JScrollPane();
        jEditorPane = new javax.swing.JEditorPane();
        javax.swing.JScrollPane jScrollPane1 = new javax.swing.JScrollPane();
        jXHyperlinkWebsite = new org.jdesktop.swingx.JXHyperlink();
        jCheckBoxChange = new javax.swing.JCheckBox();

        jCheckBoxBeschreibung.setToolTipText("Beschreibung ausblenden");

        jLabel1.setText("zur Website:");

        jScrollPane2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jEditorPane.setBorder(javax.swing.BorderFactory.createEmptyBorder(4, 4, 4, 4));
        jEditorPane.setFont(new java.awt.Font("Dialog", 0, 24)); // NOI18N
        jScrollPane2.setViewportView(jEditorPane);

        jScrollPane1.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jScrollPane1.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        jScrollPane1.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);

        jXHyperlinkWebsite.setText("jXHyperlink1");
        jScrollPane1.setViewportView(jXHyperlinkWebsite);

        jCheckBoxChange.setToolTipText("Beschreibung ändern");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 184, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jCheckBoxChange)
                .addContainerGap())
            .addComponent(jScrollPane2)
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 84, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel1)
                    .addComponent(jScrollPane1)
                    .addComponent(jCheckBoxChange))
                .addContainerGap())
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jCheckBoxBeschreibung)
                .addGap(5, 5, 5)
                .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGap(5, 5, 5))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jCheckBoxBeschreibung)
            .addGroup(layout.createSequentialGroup()
                .addGap(5, 5, 5)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox jCheckBoxBeschreibung;
    private javax.swing.JCheckBox jCheckBoxChange;
    private javax.swing.JEditorPane jEditorPane;
    private org.jdesktop.swingx.JXHyperlink jXHyperlinkWebsite;
    // End of variables declaration//GEN-END:variables
}
