/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui.history;

import mSearch.daten.DatenFilm;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.controller.history.MVUsedUrl;
import mediathek.controller.history.MVUsedUrls;
import mediathek.daten.DatenDownload;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.gui.dialog.DialogZiel;
import mediathek.gui.filmInformation.InfoDialog;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.TModel;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.List;

import static mediathek.controller.history.MVUsedUrl.TITLE_HEADER;
import static mediathek.controller.history.MVUsedUrl.USED_URL_URL;

@SuppressWarnings("serial")
public abstract class PanelErledigteUrls extends JPanel {
    protected final Daten daten;
    protected MVUsedUrls workList;

    public PanelErledigteUrls(Daten d) {
        this.daten = d;

        initComponents();
        jTable1.addMouseListener(new BeobMausTabelle());
        jButtonLoeschen.setEnabled(false);
        jButtonExport.addActionListener((ActionEvent e) -> export());
        initListeners();
    }

    protected void changeListHandler() {
        if (jToggleButtonLaden.isSelected())
            updateModelAndRecalculate(createDataModel());
    }

    private void initListeners() {
        jToggleButtonLaden.addActionListener((ActionEvent e) -> {
            if (jToggleButtonLaden.isSelected()) {
                jButtonLoeschen.setEnabled(true);
                updateModelAndRecalculate(createDataModel());
            } else {
                jButtonLoeschen.setEnabled(false);
                updateModelAndRecalculate(new TModel(null, TITLE_HEADER));
            }
        });

        jButtonLoeschen.addActionListener((ActionEvent e) -> {
            final int ret = JOptionPane.showConfirmDialog(this, "Alle Einträge werden gelöscht.", "Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                workList.alleLoeschen();
            }
        });
    }

    private void updateModelAndRecalculate(@NotNull TModel model) {
        jTable1.setModel(model);
        setsum();
    }

    protected TModel createDataModel() {
        return new TModel(workList.getObjectData(), TITLE_HEADER);
    }

    private void setsum() {
        if (jTable1.getRowCount() <= 0) {
            jLabelSum.setText("");
        } else {
            jLabelSum.setText("Anzahl: " + jTable1.getRowCount());
        }
    }

    protected String getExportFileLocation() {
        DialogZiel dialog = new DialogZiel(null, true, GuiFunktionen.getHomePath() + File.separator + "Mediathek-Filme.txt", "Filmtitel speichern");
        dialog.setVisible(true);
        if (!dialog.ok)
            return "";
        else
            return dialog.ziel;

    }

    protected abstract List<MVUsedUrl> getExportableList();

    private void export() {
        if (jTable1.getModel().getRowCount() <= 0)
            return;

        final String ziel = getExportFileLocation();
        if (!ziel.isEmpty())
            new HistoryWriterThread(ziel, getExportableList()).start();

    }

    class BeobMausTabelle extends MouseAdapter {

        //rechte Maustaste in der Tabelle
        private final BeobLoeschen beobLoeschen = new BeobLoeschen();
        private final BeobUrl beobUrl = new BeobUrl();
        private Point p;
        private DatenFilm film;

        @Override
        public void mousePressed(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            p = evt.getPoint();
            int nr = jTable1.rowAtPoint(p);
            if (nr >= 0) {
                jTable1.setRowSelectionInterval(nr, nr);
                String url = jTable1.getValueAt(jTable1.convertRowIndexToModel(nr), USED_URL_URL).toString();
                film = daten.getListeFilme().getFilmByUrl(url);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();
            //löschen
            JMenuItem item = new JMenuItem("Url aus der Liste löschen");
            item.addActionListener(beobLoeschen);
            jPopupMenu.add(item);
            //Url
            item = new JMenuItem("URL kopieren");
            item.addActionListener(beobUrl);
            jPopupMenu.add(item);
            // Infos anzeigen
            item = new JMenuItem("Infos zum Film anzeigen");
            item.addActionListener(new BeobInfo());
            jPopupMenu.add(item);
            if (film == null) {
                item.setEnabled(false);
            }
            // Download anlegen
            item = new JMenuItem("Download noch einmal anlegen");
            item.addActionListener(new BeobDownload());
            jPopupMenu.add(item);
            if (film == null) {
                item.setEnabled(false);
            }
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

        class BeobDownload implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                DatenDownload datenDownload = daten.getListeDownloads().getDownloadUrlFilm(film.getUrl());
                if (datenDownload != null) {
                    int ret = JOptionPane.showConfirmDialog(getParent(), "Download für den Film existiert bereits.\n"
                            + "Noch einmal anlegen?", "Anlegen?", JOptionPane.YES_NO_OPTION);
                    if (ret != JOptionPane.OK_OPTION) {
                        return;
                    }
                }
                // weiter
                DialogAddDownload dialog = new DialogAddDownload(MediathekGui.ui(), daten, film, null, "");
                dialog.setVisible(true);
            }

        }

        class BeobInfo implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                final InfoDialog dlg = MediathekGui.ui().getFilmInfoDialog();
                dlg.updateCurrentFilm(film);
                dlg.showInfo();
            }
        }

        class BeobUrl implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                final int selectedTableRow = jTable1.getSelectedRow();
                if (selectedTableRow != -1) {
                    String del = jTable1.getValueAt(jTable1.convertRowIndexToModel(selectedTableRow), USED_URL_URL).toString();
                    GuiFunktionen.copyToClipboard(del);
                }
            }
        }

        private class BeobLoeschen implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                final int selectedTableRow = jTable1.getSelectedRow();
                if (selectedTableRow != -1) {
                    String del = jTable1.getValueAt(jTable1.convertRowIndexToModel(selectedTableRow), USED_URL_URL).toString();
                    workList.urlAusLogfileLoeschen(del);
                }
            }
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JScrollPane jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jButtonLoeschen = new javax.swing.JButton();
        jToggleButtonLaden = new javax.swing.JToggleButton();
        jButtonExport = new javax.swing.JButton();
        jLabelSum = new javax.swing.JLabel();

        jTable1.setModel(new TModel());
        jScrollPane1.setViewportView(jTable1);

        jButtonLoeschen.setText("Liste löschen");

        jToggleButtonLaden.setText("Laden");

        jButtonExport.setText("Liste exportieren");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 554, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(jToggleButtonLaden)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelSum)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonExport)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonLoeschen)))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonLoeschen, jToggleButtonLaden});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 283, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jToggleButtonLaden)
                    .addComponent(jLabelSum)
                    .addComponent(jButtonExport)
                    .addComponent(jButtonLoeschen))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonExport;
    protected javax.swing.JButton jButtonLoeschen;
    private javax.swing.JLabel jLabelSum;
    protected javax.swing.JTable jTable1;
    protected javax.swing.JToggleButton jToggleButtonLaden;
    // End of variables declaration//GEN-END:variables
}
