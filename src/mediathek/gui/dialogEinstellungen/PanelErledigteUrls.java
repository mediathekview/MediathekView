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

import java.awt.Point;
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
import java.util.Iterator;
import java.util.LinkedList;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import mediathek.controller.MVUsedUrl;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.gui.dialog.DialogZiel;
import mediathek.gui.dialog.MVFilmInformation;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVMessageDialog;
import mediathek.tool.MVRun;
import mediathek.tool.TModel;
import msearch.daten.DatenFilm;

public class PanelErledigteUrls extends PanelVorlage {

    private boolean abo;

    public PanelErledigteUrls(Daten d, JFrame parentComponent) {
        super(d, parentComponent);
        initComponents();
        jTable1.addMouseListener(new BeobMausTabelle());
        jButtonLoeschen.setEnabled(false);
        jButtonExport.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                export();
            }
        });
    }

    public void initAbo() {
        abo = true;
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_ERLEDIGTE_ABOS, PanelErledigteUrls.class.getSimpleName()) {
            @Override
            public void ping() {
                if (jToggleButtonLaden.isSelected()) {
                    jTable1.setModel(new TModel(daten.erledigteAbos.getObjectData(), MVUsedUrl.title));
                }
            }
        });
        jButtonLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.erledigteAbos.alleLoeschen();
            }
        });
        jToggleButtonLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (jToggleButtonLaden.isSelected()) {
                    jButtonLoeschen.setEnabled(true);
                    jTable1.setModel(new TModel(daten.erledigteAbos.getObjectData(), MVUsedUrl.title));
                } else {
                    jButtonLoeschen.setEnabled(false);
                    jTable1.setModel(new TModel(null, MVUsedUrl.title));
                }
            }
        });
    }

    public void initHistory() {
        abo = false;
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_HISTORY_GEAENDERT, PanelErledigteUrls.class.getSimpleName()) {
            @Override
            public void ping() {
                if (jToggleButtonLaden.isSelected()) {
                    jTable1.setModel(new TModel(daten.history.getObjectData(), MVUsedUrl.title));
                }
            }
        });
        jButtonLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.history.alleLoeschen();
            }
        });
        jToggleButtonLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (jToggleButtonLaden.isSelected()) {
                    jButtonLoeschen.setEnabled(true);
                    jTable1.setModel(new TModel(daten.history.getObjectData(), MVUsedUrl.title));
                } else {
                    jButtonLoeschen.setEnabled(false);
                    jTable1.setModel(new TModel(null, MVUsedUrl.title));
                }
            }
        });
    }

    private void export() {
        if (jTable1.getModel().getRowCount() <= 0) {
            return;
        }
        DialogZiel dialog = new DialogZiel(parentComponent, true, GuiFunktionen.getHomePath() + File.separator + "Mediathek-Filme.txt", "Filmtitel speichern");
        dialog.setVisible(true);
        if (!dialog.ok) {
            return;
        }
        new Thread(new Export_(dialog.ziel)).start();
    }

    private class Export_ implements Runnable {

        String ziel;
        MVRun mVRun;

        public Export_(final String ziel) {
            this.ziel = ziel;
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    mVRun = new MVRun(daten.mediathekGui, "Datei: \"" + ziel + "\" erstellen");
                    mVRun.setVisible(true);
                }
            });
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
                Iterator<MVUsedUrl> it = liste.iterator();
                while (it.hasNext()) {
                    bw.write(it.next().getString());
                    bw.newLine();
                }
                bw.newLine();
                //
                bw.flush();
                bw.close();
            } catch (Exception ex) {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        MVMessageDialog.showMessageDialog(null, "Datei konnte nicht geschrieben werden!",
                                "Fehler beim Schreiben", JOptionPane.ERROR_MESSAGE);
                    }
                });
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
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 492, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(jToggleButtonLaden)
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
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 278, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonLoeschen)
                    .addComponent(jToggleButtonLaden)
                    .addComponent(jButtonExport))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonExport;
    private javax.swing.JButton jButtonLoeschen;
    private javax.swing.JTable jTable1;
    private javax.swing.JToggleButton jToggleButtonLaden;
    // End of variables declaration//GEN-END:variables

    public class BeobMausTabelle extends MouseAdapter {

        //rechhte Maustaste in der Tabelle
        BeobLoeschen beobLoeschen = new BeobLoeschen();
        BeobUrl beobUrl = new BeobUrl();
        private Point p;
        DatenFilm film = null;

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
                String url = jTable1.getValueAt(jTable1.convertRowIndexToModel(nr), 3).toString();
                film = Daten.listeFilme.getFilmByUrl(url);
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
            item = new JMenuItem("Download anlegen");
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
                DatenDownload datenDownload = Daten.listeDownloads.getDownloadUrlFilm(film.arr[DatenFilm.FILM_URL_NR]);
                if (datenDownload != null) {
                    int ret = JOptionPane.showConfirmDialog(parentComponent, "Download für den Film existiert bereits.\n"
                            + "Nochmal anlegen?", "Anlegen?", JOptionPane.YES_NO_OPTION);
                    if (ret != JOptionPane.OK_OPTION) {
                        return;
                    }
                }
                // weiter
                DialogAddDownload dialog = new DialogAddDownload(daten.mediathekGui, daten, film, null, "");
                dialog.setVisible(true);
            }

        }

        private class BeobInfo implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                MVFilmInformation filmInfoHud = daten.filmInfoHud;
                filmInfoHud.updateCurrentFilm(film);
                filmInfoHud.show();
            }
        }

        private class BeobUrl implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int selectedTableRow = jTable1.getSelectedRow();
                if (selectedTableRow >= 0) {
                    String del = jTable1.getValueAt(jTable1.convertRowIndexToModel(selectedTableRow), 2).toString();
                    if (abo) {
                        GuiFunktionen.copyToClipboard(del);
                    } else {
                        GuiFunktionen.copyToClipboard(del);
                    }
                }

            }
        }

        private class BeobLoeschen implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int selectedTableRow = jTable1.getSelectedRow();
                if (selectedTableRow >= 0) {
                    String del = jTable1.getValueAt(jTable1.convertRowIndexToModel(selectedTableRow), 2).toString();
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
