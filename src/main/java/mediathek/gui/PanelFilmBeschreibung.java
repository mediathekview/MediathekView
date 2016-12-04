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

import mSearch.daten.DatenFilm;
import mSearch.tool.DbgMsg;
import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.daten.DatenDownload;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.gui.dialog.DialogFilmBeschreibung;
import mediathek.tool.BeobMausUrl;
import mediathek.tool.MVFont;
import mediathek.tool.MVTable;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableModel;
import java.net.URISyntaxException;

@SuppressWarnings("serial")
public class PanelFilmBeschreibung extends JPanel implements ListSelectionListener {
    private DatenFilm currentFilm = null;
    private MVTable table = null;

    public PanelFilmBeschreibung(Daten daten, MVTable table, boolean film) {
        initComponents();
        this.table = table;

        jCheckBoxBeschreibung.setIcon(Icons.ICON_CHECKBOX_CLOSE);
        jCheckBoxBeschreibung.addActionListener(e -> {
            if (film) {
                MVConfig.add(MVConfig.Configs.SYSTEM_FILME_BESCHREIBUNG_ANZEIGEN, Boolean.FALSE.toString());
                Listener.notify(Listener.EREIGNIS_FILM_BESCHREIBUNG_ANZEIGEN, PanelFilmBeschreibung.class.getSimpleName());
            } else {
                MVConfig.add(MVConfig.Configs.SYSTEM_DOWNOAD_BESCHREIBUNG_ANZEIGEN, Boolean.FALSE.toString());
                Listener.notify(Listener.EREIGNIS_DOWNLOAD_BESCHREIBUNG_ANZEIGEN, PanelFilmBeschreibung.class.getSimpleName());
            }
        });

        try {
            jXHyperlinkWebsite.setAction(new UrlHyperlinkAction(daten.getMediathekGui(), ""));
        } catch (URISyntaxException ignored) {
            jXHyperlinkWebsite.setText("");
        }
        jXHyperlinkWebsite.addMouseListener(new BeobMausUrl(jXHyperlinkWebsite));

        jCheckBoxChange.setIcon(Icons.ICON_CHECKBOX_EDIT);
        jCheckBoxChange.addActionListener(e -> {
            if (currentFilm != null) {
                final String akt = currentFilm.arr[DatenFilm.FILM_BESCHREIBUNG];
                new DialogFilmBeschreibung(daten.getMediathekGui(), daten, currentFilm).setVisible(true);
                if (!currentFilm.arr[DatenFilm.FILM_BESCHREIBUNG].equals(akt)) {
                    // dann hat sich die Beschreibung geändert
                    setText();
                    daten.filmlisteSpeichern();
                    Listener.notify(Listener.EREIGNIS_BESCHREIBUNG, PanelFilmBeschreibung.class.getSimpleName());
                }
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_FONT, PanelFilmBeschreibung.class.getSimpleName()) {
            @Override
            public void ping() {
                setText();
            }
        });

        table.getSelectionModel().addListSelectionListener(this);

        //update for first time...
        updateFilmData();
    }

    private void updateFilmData() {
        final int selectedTableRow = table.getSelectedRow();
        if (selectedTableRow >= 0) {
            DatenFilm film;
            final TableModel model = table.getModel();
            final int modelIndex = table.convertRowIndexToModel(selectedTableRow);

            switch (table.getTableType()) {
                case FILME:
                    film = (DatenFilm) model.getValueAt(modelIndex, DatenFilm.FILM_REF);
                    break;

                case DOWNLOADS:
                    film = ((DatenDownload) model.getValueAt(modelIndex, DatenDownload.DOWNLOAD_REF)).film;
                    break;

                default:
                    DbgMsg.print("UNHANDLED TABLE TYPE!!!");
                    film = null;
                    break;
            }
            displayFilmData(film);
        } else {
            displayFilmData(null);
        }
    }

    private void displayFilmData(DatenFilm aaktFilm) {
        currentFilm = aaktFilm;
        setText();
    }

    private void setText() {
        if (currentFilm == null) {
            jEditorPane.setText("");
            jXHyperlinkWebsite.setText("");
        } else {
            // Beschreibung setzen
            jEditorPane.setText(
                    "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
                    + "<head><style type=\"text/css\">.sans { font-family: Verdana, Geneva, sans-serif; font-size: " + MVFont.fontSize + "pt; }</style></head>\n"
                    + "<body>"
                    + "<span class=\"sans\"><b>" + (currentFilm.arr[DatenFilm.FILM_SENDER].isEmpty() ? "" : currentFilm.arr[DatenFilm.FILM_SENDER] + "  -  ")
                    + currentFilm.arr[DatenFilm.FILM_TITEL] + "</b><br /></span>"
                    + "<span class=\"sans\">" + currentFilm.arr[DatenFilm.FILM_BESCHREIBUNG].replace("\n", "<br />") + "</span>"
                    + "</body>"
                    + "</html>");

            jXHyperlinkWebsite.setText(currentFilm.arr[DatenFilm.FILM_WEBSEITE]);
        }
    }

    @Override
    public void valueChanged(ListSelectionEvent e) {
        if (!e.getValueIsAdjusting()) {
            updateFilmData();
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

        jEditorPane.setEditable(false);
        jEditorPane.setBorder(javax.swing.BorderFactory.createEmptyBorder(4, 4, 4, 4));
        jEditorPane.setContentType("text/html"); // NOI18N
        jEditorPane.setFont(new java.awt.Font("Dialog", 0, 24)); // NOI18N
        jScrollPane2.setViewportView(jEditorPane);

        jScrollPane1.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jScrollPane1.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        jScrollPane1.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);

        jXHyperlinkWebsite.setText("");
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
