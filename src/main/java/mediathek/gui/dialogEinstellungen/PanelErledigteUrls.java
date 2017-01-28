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
package mediathek.gui.dialogEinstellungen;

import mSearch.daten.DatenFilm;
import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.controller.MVUsedUrl;
import mediathek.daten.DatenDownload;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.gui.dialog.DialogZiel;
import mediathek.gui.filmInformation.IFilmInformation;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVMessageDialog;
import mediathek.tool.MVRun;
import mediathek.tool.TModel;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedList;

@SuppressWarnings("serial")
public class PanelErledigteUrls extends PanelVorlage {
    private boolean abo;

    public PanelErledigteUrls(Daten d, JFrame parentComponent) {
        super(d, parentComponent);
        initComponents();
        jTable1.addMouseListener(new BeobMausTabelle());
        jButtonLoeschen.setEnabled(false);
        jButtonExport.addActionListener((ActionEvent e) -> export());
    }

    public void initAbo() {
        abo = true;
        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_ERLEDIGTE_ABOS, PanelErledigteUrls.class.getSimpleName()) {
            @Override
            public void ping() {
                if (jToggleButtonLaden.isSelected()) {
                    jTable1.setModel(new TModel(daten.erledigteAbos.getObjectData(), MVUsedUrl.title));
                    setsum();
                }
            }
        });
        jButtonLoeschen.addActionListener((ActionEvent e) -> {
            int ret = JOptionPane.showConfirmDialog(parentComponent, "Alle Einträge werden gelöscht.", "Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                daten.erledigteAbos.alleLoeschen();
            }
        });
        jToggleButtonLaden.addActionListener((ActionEvent e) -> {
            if (jToggleButtonLaden.isSelected()) {
                jButtonLoeschen.setEnabled(true);
                jTable1.setModel(new TModel(daten.erledigteAbos.getObjectData(), MVUsedUrl.title));
                setsum();
            } else {
                jButtonLoeschen.setEnabled(false);
                jTable1.setModel(new TModel(null, MVUsedUrl.title));
                setsum();
            }
        });
    }

    public void initHistory() {
        abo = false;
        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_HISTORY_GEAENDERT, PanelErledigteUrls.class.getSimpleName()) {
            @Override
            public void ping() {
                if (jToggleButtonLaden.isSelected()) {
                    jTable1.setModel(new TModel(daten.history.getObjectData(), MVUsedUrl.title));
                    setsum();
                }
            }
        });
        jButtonLoeschen.addActionListener((ActionEvent e) -> {
            int ret = JOptionPane.showConfirmDialog(parentComponent, "Alle Einträge werden gelöscht.", "Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                daten.history.alleLoeschen();
            }
        });
        jToggleButtonLaden.addActionListener((ActionEvent e) -> {
            if (jToggleButtonLaden.isSelected()) {
                jButtonLoeschen.setEnabled(true);
                jTable1.setModel(new TModel(daten.history.getObjectData(), MVUsedUrl.title));
                setsum();
            } else {
                jButtonLoeschen.setEnabled(false);
                jTable1.setModel(new TModel(null, MVUsedUrl.title));
                setsum();
            }
        });
    }

    private void setsum() {
        if (jTable1.getRowCount() <= 0) {
            jLabelSum.setText("");
        } else {
            jLabelSum.setText("Anzahl: " + jTable1.getRowCount());
        }
    }

    private void export() {
        MVRun mVRun;
        if (jTable1.getModel().getRowCount() <= 0) {
            return;
        }
        DialogZiel dialog = new DialogZiel(parentComponent, true, GuiFunktionen.getHomePath() + File.separator + "Mediathek-Filme.txt", "Filmtitel speichern");
        dialog.setVisible(true);
        if (!dialog.ok) {
            return;
        }
        mVRun = new MVRun(daten.getMediathekGui(), "Datei: \"" + dialog.ziel + "\" erstellen");
        mVRun.setVisible(true);
        new Thread(new Export_(dialog.ziel, mVRun)).start();
    }

    private class Export_ implements Runnable {

        String ziel;
        MVRun mVRun;

        public Export_(final String ziel, MVRun mVRun) {
            this.ziel = ziel;
            this.mVRun = mVRun;
        }

        @Override
        public synchronized void run() {
            exportListe();
        }

        private void exportListe() {
            LinkedList<MVUsedUrl> liste;
            if (abo) {
                liste = daten.erledigteAbos.getSortList();
            } else {
                liste = daten.history.getSortList();
            }
            Path logFilePath = Paths.get(ziel);
            try (BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(logFilePath)))) {
                bw.newLine();
                bw.write(MVUsedUrl.getHeaderString());
                bw.newLine();
                bw.newLine();
                for (MVUsedUrl entry : liste) {
                    bw.write(entry.getString());
                    bw.newLine();
                }
                bw.newLine();
                //
                bw.flush();
            } catch (Exception ex) {
                SwingUtilities.invokeLater(() -> MVMessageDialog.showMessageDialog(null, "Datei konnte nicht geschrieben werden!",
                        "Fehler beim Schreiben", JOptionPane.ERROR_MESSAGE));
            } finally {
                mVRun.dispose();
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
    private javax.swing.JButton jButtonLoeschen;
    private javax.swing.JLabel jLabelSum;
    private javax.swing.JTable jTable1;
    private javax.swing.JToggleButton jToggleButtonLaden;
    // End of variables declaration//GEN-END:variables

    public class BeobMausTabelle extends MouseAdapter {

        //rechhte Maustaste in der Tabelle
        BeobLoeschen beobLoeschen = new BeobLoeschen();
        BeobUrl beobUrl = new BeobUrl();
        private Point p;
        DatenFilm film;

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
                String url = jTable1.getValueAt(jTable1.convertRowIndexToModel(nr), MVUsedUrl.USED_URL_URL).toString();
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

        private class BeobDownload implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                DatenDownload datenDownload = daten.getListeDownloads().getDownloadUrlFilm(film.arr[DatenFilm.FILM_URL]);
                if (datenDownload != null) {
                    int ret = JOptionPane.showConfirmDialog(parentComponent, "Download für den Film existiert bereits.\n"
                            + "Noch einmal anlegen?", "Anlegen?", JOptionPane.YES_NO_OPTION);
                    if (ret != JOptionPane.OK_OPTION) {
                        return;
                    }
                }
                // weiter
                DialogAddDownload dialog = new DialogAddDownload(daten.getMediathekGui(), daten, film, null, "");
                dialog.setVisible(true);
            }

        }

        private class BeobInfo implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                IFilmInformation hud = daten.getMediathekGui().getFilmInformationHud();
                hud.updateCurrentFilm(film);
                hud.showInfo();
            }
        }

        private class BeobUrl implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int selectedTableRow = jTable1.getSelectedRow();
                if (selectedTableRow >= 0) {
                    String del = jTable1.getValueAt(jTable1.convertRowIndexToModel(selectedTableRow), MVUsedUrl.USED_URL_URL).toString();
                    GuiFunktionen.copyToClipboard(del);
                }

            }
        }

        private class BeobLoeschen implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int selectedTableRow = jTable1.getSelectedRow();
                if (selectedTableRow >= 0) {
                    String del = jTable1.getValueAt(jTable1.convertRowIndexToModel(selectedTableRow), MVUsedUrl.USED_URL_URL).toString();
                    if (abo) {
                        daten.erledigteAbos.urlAusLogfileLoeschen(del);
                    } else {
                        daten.history.urlAusLogfileLoeschen(del);
                    }
                }
            }
        }
    }
}
