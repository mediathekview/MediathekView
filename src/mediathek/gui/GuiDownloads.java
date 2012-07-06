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

import java.awt.Point;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import mediathek.Daten;
import mediathek.MediathekGui;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.controller.io.starter.StartEvent;
import mediathek.controller.io.starter.StartListener;
import mediathek.controller.io.starter.Starts;
import mediathek.daten.DDaten;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.gui.beobachter.BeobMpanel;
import mediathek.gui.beobachter.CellRendererDownloads;
import mediathek.gui.dialog.DialogDatenFilm;
import mediathek.gui.dialog.DialogEditDownload;
import mediathek.tool.*;

public class GuiDownloads extends PanelVorlage {

    private DialogDatenFilm dialogDatenFilm = null;
    private JTableMed tabelle;

    public GuiDownloads(DDaten d) {
        super(d);
        initComponents();
        tabelle = new JTableMed(JTableMed.TABELLE_TAB_DOWNLOADS);
        jScrollPane1.setViewportView(tabelle);
        dialogDatenFilm = new DialogDatenFilm(null, false, ddaten);
        init();
        load();
        tabelle.initTabelle();
    }
    //===================================
    //public
    //===================================

    @Override
    public void isShown() {
        super.isShown();
        ddaten.mediathekGui.setToolbar(MediathekGui.ButtonDonwload);
        ddaten.infoPanel.setIdx(InfoPanel.IDX_GUI_DOWNLOAD);
    }

    //toolbar
    public void akualisieren() {
        Daten.setGeaendert();
        aufraeumen();
        ddaten.listeDownloads.abosLoschen();
        ddaten.listeDownloads.abosEintragen();
        load();
    }

    public void starten() {
        downloadAll("");
    }

    public void zurueckstellen() {
        downloadLoeschen(false);
    }

    public void loeschen() {
        downloadLoeschen(true);
    }

    public void aufraeumen() {
        tabelleAufraeumen();
    }

    public void aendern() {
        downloadAendern();
    }

    public void stoppen() {
        filmStartenWiederholenStoppen(false /* starten */);
    }

    public void alleStoppen() {
        stopAll();
    }

    //===================================
    //private
    //===================================
    private void init() {
        Daten.addAdListener(new MediathekListener(MediathekListener.EREIGNIS_LISTE_DOWNLOADS, GuiDownloads.class.getSimpleName()) {

            @Override
            public void ping() {
                load();
            }
        });
        jRadioButtonAbos.setForeground(GuiKonstanten.ABO_FOREGROUND);
        jRadioButtonDownloads.setForeground(GuiKonstanten.DOWNLOAD_FOREGROUND);
        tabelle.setDefaultRenderer(Object.class, new CellRendererDownloads(ddaten));
        tabelle.setDefaultRenderer(Datum.class, new CellRendererDownloads(ddaten));
        tabelle.setModel(new TModelDownload(new Object[][]{}, DatenDownload.DOWNLOAD_COLUMN_NAMES));
        tabelle.addMouseListener(new BeobMausTabelle());
        tabelle.getSelectionModel().addListSelectionListener(new BeobachterTableSelect1());
        //aendern
        ActionMap am = tabelle.getActionMap();
        am.put("aendern", new BeobAbstractAction());
        InputMap im = tabelle.getInputMap();
        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
        im.put(enter, "aendern");
        //
        ddaten.starterClass.addListener(new BeobachterStart());
        jRadioButtonAlles.addActionListener(new BeobAnzeige());
        jRadioButtonAbos.addActionListener(new BeobAnzeige());
        jRadioButtonDownloads.addActionListener(new BeobAnzeige());
        jCheckBoxFilter.addActionListener(new BeobMpanel(jCheckBoxFilter, jPanelFilter, "Filter"));
    }

    private void load() {
        //Filme laden
        boolean abo, download;
        getSpalten(tabelle);
        if (jRadioButtonAlles.isSelected()) {
            abo = true;
            download = true;
        } else if (jRadioButtonAbos.isSelected()) {
            abo = true;
            download = false;
        } else {
            abo = false;
            download = true;
        }
        ddaten.listeDownloads.getModel((TModelDownload) tabelle.getModel(), abo, download);
        ((TModelDownload) tabelle.getModel()).fireTableDataChanged();
        setSpalten(tabelle);
        setInfo();
    }

    private void downloadAendern() {
        int rows[] = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int i = rows.length - 1; i >= 0; --i) {
                int delRow = tabelle.convertRowIndexToModel(rows[i]);
                String url = tabelle.getModel().getValueAt(delRow, DatenDownload.DOWNLOAD_URL_NR).toString();
                DatenDownload download = ddaten.listeDownloads.getDownloadByUrl(url);
                DatenDownload d = download.getCopy();
                DialogEditDownload dialog = new DialogEditDownload(null, true, d);
                dialog.setVisible(true);
                if (dialog.ok) {
                    download.aufMichKopieren(d);
                    DDaten.setGeaendert();
                    load();
                }
            }
            setInfo();
        } else {
            new HinweisKeineAuswahl().zeigen();
        }
    }

    private void downloadLoeschen(boolean dauerhaft) {
        int rows[] = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int i = rows.length - 1; i >= 0; --i) {
                int delRow = tabelle.convertRowIndexToModel(rows[i]);
                String url = tabelle.getModel().getValueAt(delRow, DatenDownload.DOWNLOAD_URL_NR).toString();
                DatenDownload download = ddaten.listeDownloads.getDownloadByUrl(url);
                if (dauerhaft) {
                    if (download.istAbo()) {
                        // ein Abo wird zusätzlich ins Logfile geschrieben
                        ddaten.erledigteAbos.zeileSchreiben(download.arr[DatenDownload.DOWNLOAD_THEMA_NR],
                                download.arr[DatenDownload.DOWNLOAD_TITEL_NR],
                                url);
                    }
                    ddaten.listeDownloads.delDownloadByUrl(url);
                }
                ddaten.starterClass.filmLoeschen(tabelle.getModel().getValueAt(delRow, DatenDownload.DOWNLOAD_URL_NR).toString());
                ((TModelDownload) tabelle.getModel()).removeRow(delRow);
            }
            setInfo();
        } else {
            new HinweisKeineAuswahl().zeigen();
        }
    }

    private void filmStartenWiederholenStoppen(boolean starten /* starten/wiederstarten oder stoppen */) {
        // ein Film der noch keinen Starts hat wird gestartet
        // ein Film dessen Starts schon auf fertig/fehler steht wird wieder gestartet
        int row = tabelle.getSelectedRow();
        if (row >= 0) {
            int delRow = tabelle.convertRowIndexToModel(row);
            String url = tabelle.getModel().getValueAt(delRow, DatenDownload.DOWNLOAD_URL_NR).toString();
            Starts s = ddaten.starterClass.getStart(url);
            if (s != null) {
                if (starten && s.status > Starts.STATUS_RUN || !starten && s.status <= Starts.STATUS_RUN) {
                    //daten.starterClass.delStart(url);
                    ddaten.starterClass.filmLoeschen(url);
                    if (s.datenDownload.istAbo()) {
                        // bei Abos Url auch aus dem Logfile löschen, der Film ist damit wieder auf "Anfang"
                        ddaten.erledigteAbos.urlAusLogfileLoeschen(url);
                    }
                }
            }
            if (starten) {
                downloadAll(url);
            }
            setInfo();
        } else {
            new HinweisKeineAuswahl().zeigen();
        }
    }

    private void stopAll() {
        // es werden alle laufenden Downloads gestopt
        for (int i = 0; i < tabelle.getRowCount(); ++i) {
            int delRow = tabelle.convertRowIndexToModel(i);
            String url = tabelle.getModel().getValueAt(delRow, DatenDownload.DOWNLOAD_URL_NR).toString();
            Starts s = ddaten.starterClass.getStart(url);
            if (s != null) {
                if (s.status <= Starts.STATUS_RUN) {
                    ddaten.starterClass.filmLoeschen(url);
                }
            }
            setInfo();
        }
    }

    private void stopWartende() {
        // es werden alle noch nicht gestarteten Downloads gelöscht
        for (int i = 0; i < tabelle.getRowCount(); ++i) {
            int delRow = tabelle.convertRowIndexToModel(i);
            String url = tabelle.getModel().getValueAt(delRow, DatenDownload.DOWNLOAD_URL_NR).toString();
            Starts s = ddaten.starterClass.getStart(url);
            if (s != null) {
                if (s.status < Starts.STATUS_RUN) {
                    ddaten.starterClass.filmLoeschen(url);
                }
            }
            setInfo();
        }
    }

    private void tabelleAufraeumen() {
        for (int i = 0; i < tabelle.getModel().getRowCount(); ++i) {
            int delRow = tabelle.convertRowIndexToModel(i);
            String url = tabelle.getModel().getValueAt(delRow, DatenDownload.DOWNLOAD_URL_NR).toString();
            Starts s = ddaten.starterClass.getStart(url);
            if (s != null) {
                if (s.status >= Starts.STATUS_FERTIG) {
                    ddaten.listeDownloads.delDownloadByUrl(url);
                    ((TModelDownload) tabelle.getModel()).removeRow(i);
                    --i;
                }
            }
        }
        setInfo();
        ddaten.starterClass.aufraeumen();
    }

    private boolean downloadAll(String uurl) {
        // liefert false, wenn es nichts zu Laden gibt
        boolean ret = false;
        String url;
        if (tabelle.getModel() != null) {
            for (int i = 0; i < tabelle.getModel().getRowCount(); ++i) {
                url = (tabelle.getModel().getValueAt(i, DatenDownload.DOWNLOAD_URL_NR).toString());
                if (uurl.equals("") || uurl.equals(url)) {
                    //Start erstellen und zur Liste hinzufügen
                    DatenDownload download = ddaten.listeDownloads.getDownloadByUrl(url);
                    ddaten.starterClass.addStarts(new Starts(download));
                    ret = true;
                }
            }
        }
        setInfo();
        return ret;
    }

    private void panelUpdate() {
        setInfo();
        tabelle.repaint();
        this.validate();
    }

    private void setInfo() {
        String textLinks;
        // Text links: Zeilen Tabelle
        int laufen = ddaten.starterClass.getDownloadsLaufen();
        int warten = ddaten.starterClass.getDownloadsWarten();
        int gesamt = tabelle.getModel().getRowCount();
        if (gesamt == 1) {
            textLinks = "1 Download,";
        } else {
            textLinks = gesamt + " Downloads,";
        }
        textLinks += " (";
        if (laufen == 1) {
            textLinks += "1 läuft,";
        } else {
            textLinks += laufen + " laufen,";
        }
        if (warten == 1) {
            textLinks += " 1 wartet";
        } else {
            textLinks += " " + warten + " warten";
        }
        textLinks += ")";
        // Infopanel setzen
        ddaten.infoPanel.setTextLinks(InfoPanel.IDX_GUI_DOWNLOAD, textLinks);
    }

    private void table1Select() {
        DatenFilm aktFilm = new DatenFilm();
        int selectedTableRow = tabelle.getSelectedRow();
        if (selectedTableRow >= 0) {
            int selectedModelRow = tabelle.convertRowIndexToModel(selectedTableRow);
            DatenFilm film = Daten.listeFilme.getFilmByUrl(tabelle.getModel().getValueAt(selectedModelRow, DatenDownload.DOWNLOAD_URL_NR).toString());
            if (film != null) {
                aktFilm = film;
            }
        }
        dialogDatenFilm.setAktFilm(aktFilm);
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonGroup1 = new javax.swing.ButtonGroup();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jPanelFilter = new javax.swing.JPanel();
        jCheckBoxFilter = new javax.swing.JCheckBox();
        jPanelFilterInnen = new javax.swing.JPanel();
        jRadioButtonAlles = new javax.swing.JRadioButton();
        jRadioButtonDownloads = new javax.swing.JRadioButton();
        jRadioButtonAbos = new javax.swing.JRadioButton();

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTable1);

        jPanelFilter.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jCheckBoxFilter.setBackground(new java.awt.Color(217, 217, 217));
        jCheckBoxFilter.setFont(new java.awt.Font("Dialog", 1, 10)); // NOI18N
        jCheckBoxFilter.setText("Filter");

        buttonGroup1.add(jRadioButtonAlles);
        jRadioButtonAlles.setSelected(true);
        jRadioButtonAlles.setText(" alle");
        jRadioButtonAlles.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)));
        jRadioButtonAlles.setBorderPainted(true);

        buttonGroup1.add(jRadioButtonDownloads);
        jRadioButtonDownloads.setText(" Downloads ");
        jRadioButtonDownloads.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)));
        jRadioButtonDownloads.setBorderPainted(true);

        buttonGroup1.add(jRadioButtonAbos);
        jRadioButtonAbos.setText(" Abos ");
        jRadioButtonAbos.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)));
        jRadioButtonAbos.setBorderPainted(true);

        javax.swing.GroupLayout jPanelFilterInnenLayout = new javax.swing.GroupLayout(jPanelFilterInnen);
        jPanelFilterInnen.setLayout(jPanelFilterInnenLayout);
        jPanelFilterInnenLayout.setHorizontalGroup(
            jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFilterInnenLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jRadioButtonAlles)
                .addGap(18, 18, 18)
                .addComponent(jRadioButtonDownloads)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jRadioButtonAbos)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelFilterInnenLayout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jRadioButtonAbos, jRadioButtonAlles, jRadioButtonDownloads});

        jPanelFilterInnenLayout.setVerticalGroup(
            jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFilterInnenLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jRadioButtonAlles)
                    .addComponent(jRadioButtonDownloads)
                    .addComponent(jRadioButtonAbos))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanelFilterLayout = new javax.swing.GroupLayout(jPanelFilter);
        jPanelFilter.setLayout(jPanelFilterLayout);
        jPanelFilterLayout.setHorizontalGroup(
            jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanelFilterInnen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(jCheckBoxFilter, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        jPanelFilterLayout.setVerticalGroup(
            jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFilterLayout.createSequentialGroup()
                .addComponent(jCheckBoxFilter)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelFilterInnen, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 670, Short.MAX_VALUE)
                    .addComponent(jPanelFilter, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelFilter, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 357, Short.MAX_VALUE)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JCheckBox jCheckBoxFilter;
    private javax.swing.JPanel jPanelFilter;
    private javax.swing.JPanel jPanelFilterInnen;
    private javax.swing.JRadioButton jRadioButtonAbos;
    private javax.swing.JRadioButton jRadioButtonAlles;
    private javax.swing.JRadioButton jRadioButtonDownloads;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTable1;
    // End of variables declaration//GEN-END:variables

    private class BeobachterStart implements StartListener {

        @Override
        public void starter(StartEvent ev) {
            panelUpdate();
        }
    }

    private class BeobachterTableSelect1 implements ListSelectionListener {

        public int selectedModelRow = -1;

        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (!event.getValueIsAdjusting()) {
                table1Select();
            }
        }
    }

    public class BeobMausTabelle extends MouseAdapter {

        private Point p;

        public BeobMausTabelle() {
        }

        @Override
        public void mouseClicked(MouseEvent arg0) {
            if (arg0.getButton() == MouseEvent.BUTTON1) {
                if (arg0.getClickCount() > 1) {
                    downloadAendern();
                }
            } else if (arg0.getButton() == MouseEvent.BUTTON3) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            p = evt.getPoint();
            int nr = tabelle.rowAtPoint(p);
            if (nr >= 0) {
                tabelle.setRowSelectionInterval(nr, nr);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();
            //Film vorziehen
            int row = tabelle.getSelectedRow();
            boolean wartenOderLaufen = false;
            if (row >= 0) {
                int delRow = tabelle.convertRowIndexToModel(row);
                Starts s = ddaten.starterClass.getStart(tabelle.getModel().getValueAt(delRow, DatenDownload.DOWNLOAD_URL_NR).toString());
                if (s != null) {
                    if (s.status <= Starts.STATUS_RUN) {
                        wartenOderLaufen = true;
                    }
                }
            }
            JMenuItem itemStarten = new JMenuItem("Download starten");
            itemStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_play_16.png")));
            itemStarten.setEnabled(!wartenOderLaufen);
            jPopupMenu.add(itemStarten);
            itemStarten.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent arg0) {
                    filmStartenWiederholenStoppen(true /* starten */);
                }
            });
            JMenuItem itemStoppen = new JMenuItem("Download stoppen");
            itemStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_stop_16.png")));
            itemStoppen.setEnabled(wartenOderLaufen);
            jPopupMenu.add(itemStoppen);
            itemStoppen.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent arg0) {
                    filmStartenWiederholenStoppen(false /* starten */);
                }
            });
 
            //#######################################
            jPopupMenu.addSeparator();
            //#######################################
            
            JMenuItem itemLoeschen = new JMenuItem("Download zurückstellen");
            itemLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/undo_16.png")));
            jPopupMenu.add(itemLoeschen);
            itemLoeschen.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadLoeschen(false /* dauerhaft */);
                }
            });
            //dauerhaft löschen
            JMenuItem itemDauerhaftLoeschen = new JMenuItem("Download dauerhaft löschen");
            itemDauerhaftLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_16.png")));
            jPopupMenu.add(itemDauerhaftLoeschen);
            itemDauerhaftLoeschen.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadLoeschen(true /* dauerhaft */);
                }
            });
            //ändern
            JMenuItem itemAendern = new JMenuItem("Download Ändern");
            itemAendern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/configure_16.png")));
            jPopupMenu.add(itemAendern);
            itemAendern.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadAendern();
                }
            });

            //#######################################
            jPopupMenu.addSeparator();
            //#######################################
            
            JMenuItem itemAlleStarten = new JMenuItem("alle Downloads starten");
            itemAlleStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/next_16.png")));
            jPopupMenu.add(itemAlleStarten);
            itemAlleStarten.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadAll("");
                }
            });
            JMenuItem itemAlleStoppen = new JMenuItem("alle Downloads stoppen");
            itemAlleStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_stop_16.png")));
            jPopupMenu.add(itemAlleStoppen);
            itemAlleStoppen.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent arg0) {
                    stopAll();
                }
            });
            JMenuItem itemWartendeStoppen = new JMenuItem("wartende Downloads stoppen");
            itemWartendeStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_stop_16.png")));
            jPopupMenu.add(itemWartendeStoppen);
            itemWartendeStoppen.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent arg0) {
                    stopWartende();
                }
            });
            JMenuItem itemAktualisieren = new JMenuItem("Liste der Downloads aktualisieren");
            itemAktualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/view-refresh_16.png")));
            jPopupMenu.add(itemAktualisieren);
            itemAktualisieren.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent arg0) {
                    akualisieren();
                }
            });
            JMenuItem itemAufraeumen = new JMenuItem("Liste Aufräumen");
            itemAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit-clear_16.png")));
            jPopupMenu.add(itemAufraeumen);
            itemAufraeumen.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent arg0) {
                    aufraeumen();
                }
            });

            //#######################################
            jPopupMenu.addSeparator();
            //#######################################

            //url
            JMenuItem itemUrl = new JMenuItem("URL kopieren");
            itemUrl.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        GuiFunktionen.copyToClipboard(
                                tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr),
                                DatenDownload.DOWNLOAD_URL_NR).toString());
                    }
                }
            });
            jPopupMenu.add(itemUrl);
            //Player
            JMenuItem itemPlayer = new JMenuItem("Film abspielen");
            itemPlayer.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        DatenPset gruppe = ddaten.listePset.getPsetAbspielen();
                        if (gruppe != null) {
                            int selectedModelRow = tabelle.convertRowIndexToModel(nr);
                            DatenFilm film = Daten.listeFilme.getFilmByUrl(tabelle.getModel().getValueAt(selectedModelRow, DatenDownload.DOWNLOAD_URL_NR).toString());
                            if (film != null) {
                                // in die History eintragen
                                ddaten.history.add(film.getUrlOrg());
                                // und starten
                                ddaten.starterClass.urlStarten(gruppe, film);
                            }
                        } else {
                            JOptionPane.showMessageDialog(null, "Im Menü unter \"Datei->Optionen->Videoplayer\" ein Programm zum Abspielen festlegen.",
                                    "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
                        }
                    }
                }
            });
            jPopupMenu.add(itemPlayer);
            //Infos
            JMenuItem itemInfo = new JMenuItem("Infos anzeigen");
            itemInfo.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    dialogDatenFilm.setVis();
                }
            });
            jPopupMenu.add(itemInfo);
            // Tabellenspalten zurücksetzen
            JMenuItem item = new JMenuItem("Spalten zurücksetzen");
            item.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    tabelle.resetTabelle();
                }
            });
            jPopupMenu.add(item);
            //Menü anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }

    private class BeobAbstractAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            downloadAendern();
        }
    }

    private class BeobAnzeige implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            load();
        }
    }
}
