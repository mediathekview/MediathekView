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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URISyntaxException;
import mediathek.daten.DDaten;
import mediathek.daten.DatenFilm;
import mediathek.res.GetIcon;
import mediathek.tool.BeobMausUrl;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.UrlHyperlinkAction;

/**
 *
 * @author emil
 */
public class PanelBeschreibung extends javax.swing.JPanel {

    DDaten ddaten;

    public PanelBeschreibung(DDaten dd) {
        initComponents();
        ddaten = dd;
        //setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));
        //jCheckBoxBeschreibung.setBackground(new java.awt.Color(204, 204, 204));
        jCheckBoxBeschreibung.setIcon(GetIcon.getIcon("close_15.png"));
        jCheckBoxBeschreibung.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DDaten.system[Konstanten.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN_NR] = Boolean.FALSE.toString();
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PANEL_BESCHREIBUNG_ANZEIGEN, PanelBeschreibung.class.getSimpleName());
            }
        });
        jXHyperlinkWebsite.setText("");
        try {
            jXHyperlinkWebsite.setAction(new UrlHyperlinkAction(ddaten, ""));
        } catch (URISyntaxException ignored) {
        }
        jXHyperlinkWebsite.addMouseListener(new BeobMausUrl(jXHyperlinkWebsite));
        //jTextAreaBeschreibung.setLineWrap(true);
        //jTextAreaBeschreibung.setWrapStyleWord(true);
        jEditorPane.setEditable(false);
        jEditorPane.setFocusable(false);
        jEditorPane.setContentType("text/html");
    }

    public void setAktFilm(DatenFilm aktFilm) {
        if (aktFilm == null) {
            jEditorPane.setText("");
//            jCheckBoxBeschreibung.setText("Beschreibung");
            jXHyperlinkWebsite.setText("");
        } else {
            // Beschreibung setzen
            jEditorPane.setText(
                    "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
                    + "<head><style type=\"text/css\">.sans { font-family: Verdana, Geneva, sans-serif;}</style></head>\n"
                    + "<body>"
                    + "<span class=\"sans\"><b>" + (aktFilm.arr[DatenFilm.FILM_SENDER_NR].isEmpty() ? "" : aktFilm.arr[DatenFilm.FILM_SENDER_NR] + "  -  ")
                    + aktFilm.arr[DatenFilm.FILM_TITEL_NR] + "</b><br /></span>"
                    + "<span class=\"sans\">" + aktFilm.arr[DatenFilm.FILM_BESCHREIBUNG_NR].replace("\n", "<br />") + "</span>"
                    + "</body>"
                    + "</html>");

            if (aktFilm.arr[DatenFilm.FILM_SENDER_NR].equals("")) {
//                jCheckBoxBeschreibung.setText("Beschreibung");
            } else {
//                jCheckBoxBeschreibung.setText("Beschreibung vom Sender: " + aktFilm.arr[DatenFilm.FILM_SENDER_NR]);
            }
            jXHyperlinkWebsite.setText(aktFilm.arr[DatenFilm.FILM_WEBSEITE_NR]);
        }
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jCheckBoxBeschreibung = new javax.swing.JCheckBox();
        jPanel1 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jEditorPane = new javax.swing.JEditorPane();
        jScrollPane1 = new javax.swing.JScrollPane();
        jXHyperlinkWebsite = new org.jdesktop.swingx.JXHyperlink();

        jCheckBoxBeschreibung.setToolTipText("Ausblenden");

        jLabel1.setText("zur Website:");

        jScrollPane2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jEditorPane.setBorder(javax.swing.BorderFactory.createEmptyBorder(4, 4, 4, 4));
        jScrollPane2.setViewportView(jEditorPane);

        jScrollPane1.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jScrollPane1.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        jScrollPane1.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);

        jXHyperlinkWebsite.setText("jXHyperlink1");
        jScrollPane1.setViewportView(jXHyperlinkWebsite);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1)
                .addGap(18, 18, 18))
            .addComponent(jScrollPane2)
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 84, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel1, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE))
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
    private javax.swing.JEditorPane jEditorPane;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private org.jdesktop.swingx.JXHyperlink jXHyperlinkWebsite;
    // End of variables declaration//GEN-END:variables
}
