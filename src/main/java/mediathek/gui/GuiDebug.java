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

import com.jidesoft.utils.SystemInfo;
import java.awt.FileDialog;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.HashSet;
import java.util.Iterator;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import mSearch.Const;
import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mSearch.filmlisten.FilmlisteLesen;
import mSearch.tool.Duration;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.dialogEinstellungen.PanelFilmlisten;

@SuppressWarnings("serial")
public class GuiDebug extends JPanel {

    private final JButton[] buttonSender;
    private final String[] sender;
    private final Daten daten;

    public GuiDebug(Daten d) {
        super();
        initComponents();
        daten = d;
        sender = daten.getFilmeLaden().getSenderNamen();
        buttonSender = new JButton[sender.length];

        jPanelFilmlisteLaden.setLayout(new GridLayout(1, 1));
        jPanelFilmlisteLaden.add(new PanelFilmlisten(d, daten.getMediathekGui()));

        jPanelStarts.setLayout(new GridLayout(1, 1));
        jPanelStarts.add(new PanelInfoStarts());

        //Tab1 Sender löschen Panel füllen
        for (int i = 0; i < daten.getFilmeLaden().getSenderNamen().length; ++i) {
            buttonSender[i] = new JButton(sender[i]);
            buttonSender[i].addActionListener(new BeobSenderLoeschen(sender[i]));
        }
        addSender();
        jButtonNeuLaden.addActionListener(ae
                -> {
            daten.getListeFilme().clear();
            Duration.staticPing("Start");
            new FilmlisteLesen().readFilmListe(Daten.getDateiFilmliste(), daten.getListeFilme(), Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
            Duration.staticPing("Fertig");
            daten.getListeFilme().themenLaden();
            daten.getListeAbo().setAboFuerFilm(daten.getListeFilme(), false /*aboLoeschen*/);
            daten.getListeBlacklist().filterListe();
            Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, GuiDebug.class.getSimpleName());
        });
        jButtonAllesSpeichern.addActionListener(e
                -> {
            daten.allesSpeichern();
            daten.filmlisteSpeichern();
        });
        jButtonFilmlisteLoeschen.addActionListener(e
                -> {
            daten.getListeFilme().clear();
            daten.getListeBlacklist().filterListe();
            Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, GuiDebug.class.getSimpleName());
        });
        jButtonFehler.addActionListener(e -> Log.endMsg());
        jButtonCheck.addActionListener(e -> daten.getListeFilme().check());

        jButtonClean.addActionListener(ae -> daten.getListeFilme().cleanList());
        jToggleButtonFastAuto.addActionListener(ae
                -> {
            if (jToggleButtonFastAuto.isSelected()) {
                FilmlisteLesen.setWorkMode(FilmlisteLesen.WorkMode.FASTAUTO);
            } else {
                FilmlisteLesen.setWorkMode(FilmlisteLesen.WorkMode.NORMAL);
            }
        });
        btnPathDiff.addActionListener(new BeobPfad());
        btnDiff.addActionListener((ActionEvent e)
                -> {
            ListeFilme listeFilme = new ListeFilme();
            final HashSet<String> hash = new HashSet<>(listeFilme.size() + 1, 1);
            new FilmlisteLesen().readFilmListe(txtDiff.getText(), listeFilme, 0);

            // ==========================================
            for (DatenFilm f : listeFilme) {
                if (f.arr[DatenFilm.FILM_SENDER].equals(Const.KIKA)) {
                    // beim KIKA ändern sich die URLs laufend
                    hash.add(f.arr[DatenFilm.FILM_THEMA] + f.arr[DatenFilm.FILM_TITEL]);
                } else if (!cbkUrl.isSelected()) {
                    hash.add(f.getIndex());
                } else {
                    hash.add(DatenFilm.getUrl(f));
                }
            }

            System.out.println("---------------------");
            System.out.println("vorher: " + daten.getListeFilme().size());

            Iterator<DatenFilm> it = daten.getListeFilme().iterator();
            while (it.hasNext()) {
                DatenFilm f = it.next();
                if (f.arr[DatenFilm.FILM_SENDER].equals(Const.KIKA)) {
                    // beim KIKA ändern sich die URLs laufend
                    if (hash.contains(f.arr[DatenFilm.FILM_THEMA] + f.arr[DatenFilm.FILM_TITEL])) {
                        it.remove();
                    }
                } else if (!cbkUrl.isSelected()) {
                    if (hash.contains(f.getIndex())) {
                        it.remove();
                    }
                } else if (hash.contains(DatenFilm.getUrl(f))) {
                    it.remove();
                }
            }

            System.out.println("danach: " + daten.getListeFilme().size());
            daten.filmlisteSpeichern();
            Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, GuiDebug.class.getSimpleName());
            hash.clear();
        });

        addComponentListener(new java.awt.event.ComponentAdapter() {
            @Override
            public void componentShown(java.awt.event.ComponentEvent evt) {
                //                daten.getMediathekGui().setTabShown(MediathekGui.TABS.TAB_NIX);
                daten.getMediathekGui().getStatusBar().setIndexForLeftDisplay(MVStatusBar.StatusbarIndex.NONE);
            }
        });

        btnDelDoppelteUrls.addActionListener(e
                -> {
            System.out.println("---------------------");
            System.out.println("vorher: " + daten.getListeFilme().size());

            ListeFilme listeFilme = new ListeFilme();
            HashSet<String> hash = new HashSet<>();
            daten.getListeFilme().stream().filter(film -> !hash.contains(film.arr[DatenFilm.FILM_URL]))
                    .forEach(film
                            -> {
                        hash.add(film.arr[DatenFilm.FILM_URL]);
                        listeFilme.add(film);
                    });
            hash.clear();
            // ==========================================
            daten.setListeFilme(listeFilme);
            System.out.println("danach: " + daten.getListeFilme().size());
            daten.filmlisteSpeichern();
        });
        btnNurDoppelte.addActionListener(e
                -> {
            ListeFilme listeFilme = new ListeFilme();
            HashSet<String> hash = new HashSet<>();
            HashSet<String> hashDoppelt = new HashSet<>();
            for (DatenFilm film : daten.getListeFilme()) {
                if (hash.contains(film.arr[DatenFilm.FILM_URL])) {
                    hashDoppelt.add(film.arr[DatenFilm.FILM_URL]);
                } else {
                    hash.add(film.arr[DatenFilm.FILM_URL]);
                }
            }
            hash.clear();
            for (DatenFilm film : daten.getListeFilme()) {
                if (hashDoppelt.contains(film.arr[DatenFilm.FILM_URL])) {
                    listeFilme.add(film);
                }
            }
            hashDoppelt.clear();
            saveNewListeFilme(listeFilme);
        });
        jButtonHashOlddoppelt.addActionListener(e
                -> {
            ListeFilme listeFilme = new ListeFilme();
            HashSet<String> hash = new HashSet<>();
            HashSet<String> hashDoppelt = new HashSet<>();
            for (DatenFilm film : daten.getListeFilme()) {
                String tt = film.getIndexAddOld();
                if (hash.contains(tt)) {
                    hashDoppelt.add(tt);
                } else {
                    hash.add(tt);
                }
            }
            hash.clear();
            for (DatenFilm film : daten.getListeFilme()) {
                String tt = film.getIndexAddOld();
                if (hashDoppelt.contains(tt)) {
                    listeFilme.add(film);
                }
            }
            hashDoppelt.clear();
            saveNewListeFilme(listeFilme);
        });
        jButtonTTUrl.addActionListener(e
                -> {
            ListeFilme listeFilme = new ListeFilme();
            HashSet<String> hash = new HashSet<>();
            HashSet<String> hashDoppelt = new HashSet<>();
            for (DatenFilm film : daten.getListeFilme()) {
                String tt = film.arr[DatenFilm.FILM_THEMA].toLowerCase() + film.arr[DatenFilm.FILM_TITEL].toLowerCase() + film.arr[DatenFilm.FILM_URL];
                if (hash.contains(tt)) {
                    hashDoppelt.add(tt);
                } else {
                    hash.add(tt);
                }
            }
            hash.clear();
            for (DatenFilm film : daten.getListeFilme()) {
                String tt = film.arr[DatenFilm.FILM_THEMA].toLowerCase() + film.arr[DatenFilm.FILM_TITEL].toLowerCase() + film.arr[DatenFilm.FILM_URL];
                if (hashDoppelt.contains(tt)) {
                    listeFilme.add(film);
                }
            }
            hashDoppelt.clear();
            saveNewListeFilme(listeFilme);
        });
        jButtonOldList.addActionListener(new BeobPfadOldUrl());
        //FIXME unten auskommentierten Block an mSearch anpassen wenn stable...
        //bis dahin bleibt der Button erst mal disabled.
        jButtonAddOld.setEnabled(false);
//        jButtonAddOld.addActionListener((ActionEvent e) ->
//        {
//            String url = jTextFieldOld.getText();
//            ListeFilme listeEinsortieren = new ListeFilme();
//            new FilmlisteLesen().readFilmListe(url, listeEinsortieren, 0 /*all days*/);
//
//            HashSet<String> hash = new HashSet<>(listeEinsortieren.size() + 1, 1);
//            HashSet<String> hash2 = new HashSet<>(listeEinsortieren.size() + 1, 1);
//
//            // ==============================================
//            // nach "Thema-Titel" suchen
//            daten.getListeFilme().forEach((f) -> hash.add(f.getIndexAddOld_()));
//            listeEinsortieren.removeIf((f) -> hash.contains(f.getIndexAddOld_()));
//            DbgMsg.print("Anzahl Filme: " + daten.getListeFilme().size());
//
//            hash.clear();
//            ListeFilme lf = new ListeFilme();
//            listeEinsortieren.forEach((f) ->
//            {
//                if (hash2.contains(f.getIndexAddOld())) {
//                    lf.add(f);
//                } else {
//                    hash2.add(f.getIndexAddOld());
//                }
//            });
//            daten.setListeFilme(lf);
//            hash.clear();
//            DbgMsg.print("Anzahl Filme: " + daten.getListeFilme().size());
//
//            daten.getListeFilme().sort();
//            daten.getListeBlacklist().filterListe();
//            Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, GuiDebug.class.getSimpleName());
//        });
        jButtonLiveStreams.addActionListener(e
                -> {
            String url = jTextFieldLiveStreams.getText();
            ListeFilme tmpListe = new ListeFilme();
            new FilmlisteLesen().readFilmListe(url, tmpListe, 0 /*all days*/);
            addLive(tmpListe);
            tmpListe.clear();
            System.gc();
            daten.getListeFilme().sort();
            daten.getListeBlacklist().filterListe();
            Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, GuiDebug.class.getSimpleName());
        });
        jButtonDelLive.addActionListener(e
                -> {
            daten.getListeFilme().removeIf(f -> f.arr[DatenFilm.FILM_THEMA].equals(ListeFilme.THEMA_LIVE));
            daten.getListeBlacklist().filterListe();
            Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, GuiDebug.class.getSimpleName());
        });
    }

    public synchronized void addLive(ListeFilme listeEinsortieren) {
        // live-streams einfügen, es werde die vorhandenen ersetzt!

        if (listeEinsortieren.size() <= 0) {
            //dann wars wohl nix
            return;
        }

        Iterator<DatenFilm> it = daten.getListeFilme().iterator();
        while (it.hasNext()) {
            DatenFilm f = it.next();
            if (f.arr[DatenFilm.FILM_THEMA].equals(ListeFilme.THEMA_LIVE)) {
                it.remove();
            }
        }
        listeEinsortieren.forEach(daten.getListeFilme()::add);
    }

    private void saveNewListeFilme(final ListeFilme listeFilme) {
        System.out.println("---------------------");
        System.out.println("vorher: " + daten.getListeFilme().size());
        daten.setListeFilme(listeFilme);
        System.out.println("danach: " + daten.getListeFilme().size());
        daten.filmlisteSpeichern();
    }

    private void addSender() {
        jPanelLoeschen.removeAll();
        jPanelLoeschen.setLayout(new GridLayout(0, 5));
        int nr = 0;
        for (String aSender : sender) {
            JButton btn = buttonSender[nr];
            btn.setText(aSender);
            jPanelLoeschen.add(btn);
            ++nr;
        }
        jPanelLoeschen.repaint();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JTabbedPane jTabbedSender = new javax.swing.JTabbedPane();
        jPanelFilmlisteLaden = new javax.swing.JPanel();
        javax.swing.JPanel jPanelFilmliste = new javax.swing.JPanel();
        javax.swing.JPanel jPanelSender = new javax.swing.JPanel();
        jPanelLoeschen = new javax.swing.JPanel();
        jButtonFilmlisteLoeschen = new javax.swing.JButton();
        jButtonNeuLaden = new javax.swing.JButton();
        jButtonCheck = new javax.swing.JButton();
        jButtonClean = new javax.swing.JButton();
        javax.swing.JPanel jPanelTools = new javax.swing.JPanel();
        jButtonFehler = new javax.swing.JButton();
        jButtonAllesSpeichern = new javax.swing.JButton();
        jToggleButtonFastAuto = new javax.swing.JToggleButton();
        btnDiff = new javax.swing.JButton();
        txtDiff = new javax.swing.JTextField();
        btnPathDiff = new javax.swing.JButton();
        btnDelDoppelteUrls = new javax.swing.JButton();
        jButtonAddOld = new javax.swing.JButton();
        jTextFieldOld = new javax.swing.JTextField();
        btnNurDoppelte = new javax.swing.JButton();
        jButtonHashOlddoppelt = new javax.swing.JButton();
        jButtonTTUrl = new javax.swing.JButton();
        jButtonLiveStreams = new javax.swing.JButton();
        jTextFieldLiveStreams = new javax.swing.JTextField();
        jButtonDelLive = new javax.swing.JButton();
        jButtonOldList = new javax.swing.JButton();
        cbkUrl = new javax.swing.JCheckBox();
        jPanelStarts = new javax.swing.JPanel();

        javax.swing.GroupLayout jPanelFilmlisteLadenLayout = new javax.swing.GroupLayout(jPanelFilmlisteLaden);
        jPanelFilmlisteLaden.setLayout(jPanelFilmlisteLadenLayout);
        jPanelFilmlisteLadenLayout.setHorizontalGroup(
                jPanelFilmlisteLadenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 725, Short.MAX_VALUE)
        );
        jPanelFilmlisteLadenLayout.setVerticalGroup(
                jPanelFilmlisteLadenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 419, Short.MAX_VALUE)
        );

        jTabbedSender.addTab("Filmliste laden", jPanelFilmlisteLaden);

        jPanelSender.setBorder(javax.swing.BorderFactory.createTitledBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED), "Sender löschen"));

        javax.swing.GroupLayout jPanelLoeschenLayout = new javax.swing.GroupLayout(jPanelLoeschen);
        jPanelLoeschen.setLayout(jPanelLoeschenLayout);
        jPanelLoeschenLayout.setHorizontalGroup(
                jPanelLoeschenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanelLoeschenLayout.setVerticalGroup(
                jPanelLoeschenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 0, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanelSenderLayout = new javax.swing.GroupLayout(jPanelSender);
        jPanelSender.setLayout(jPanelSenderLayout);
        jPanelSenderLayout.setHorizontalGroup(
                jPanelSenderLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelSenderLayout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jPanelLoeschen, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanelSenderLayout.setVerticalGroup(
                jPanelSenderLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelSenderLayout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jPanelLoeschen, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addContainerGap(285, Short.MAX_VALUE))
        );

        jButtonFilmlisteLoeschen.setText("gesamte Filmliste löschen");

        jButtonNeuLaden.setText("gespeicherte Filmliste neu laden");

        jButtonCheck.setText("Check Filmliste");

        jButtonClean.setText("Clean Filmliste");

        javax.swing.GroupLayout jPanelFilmlisteLayout = new javax.swing.GroupLayout(jPanelFilmliste);
        jPanelFilmliste.setLayout(jPanelFilmlisteLayout);
        jPanelFilmlisteLayout.setHorizontalGroup(
                jPanelFilmlisteLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelFilmlisteLayout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanelFilmlisteLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jPanelSender, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addGroup(jPanelFilmlisteLayout.createSequentialGroup()
                                                .addGroup(jPanelFilmlisteLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                                        .addGroup(jPanelFilmlisteLayout.createSequentialGroup()
                                                                .addComponent(jButtonFilmlisteLoeschen)
                                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                                                .addComponent(jButtonNeuLaden))
                                                        .addGroup(jPanelFilmlisteLayout.createSequentialGroup()
                                                                .addComponent(jButtonCheck)
                                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                                                .addComponent(jButtonClean, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                                                .addGap(0, 0, Short.MAX_VALUE)))
                                .addContainerGap())
        );

        jPanelFilmlisteLayout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[]{jButtonCheck, jButtonFilmlisteLoeschen, jButtonNeuLaden});

        jPanelFilmlisteLayout.setVerticalGroup(
                jPanelFilmlisteLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelFilmlisteLayout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanelFilmlisteLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(jButtonFilmlisteLoeschen)
                                        .addComponent(jButtonNeuLaden))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanelFilmlisteLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(jButtonCheck)
                                        .addComponent(jButtonClean))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jPanelSender, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addContainerGap())
        );

        jTabbedSender.addTab("Filmliste", jPanelFilmliste);

        jButtonFehler.setText("Fehler ausgeben");

        jButtonAllesSpeichern.setText("alles speichern");

        jToggleButtonFastAuto.setText("-FASTAUTO");

        btnDiff.setText("was fehlt in der Liste:");

        txtDiff.setText("/home/emil/Desktop/thschüss/filmlisten/alex/filme.json-summe");

        btnPathDiff.setText(":::");

        btnDelDoppelteUrls.setText("doppelte URLs löschen");

        jButtonAddOld.setText("Alte Filmliste");

        jTextFieldOld.setText("/tmp/usb/2016-09-10-filme.xz");

        btnNurDoppelte.setText("NUR doppelte URLs");

        jButtonHashOlddoppelt.setText("NUR doppelte HashOld");

        jButtonTTUrl.setText("NUR doppelte Th+Ti+Url");

        jButtonLiveStreams.setText("Live-Streams einfügen");

        jTextFieldLiveStreams.setText("http://zdfmediathk.sourceforge.net/live-streams.json");

        jButtonDelLive.setText("löschen");

        jButtonOldList.setText(":::");

        cbkUrl.setText("nur URL");

        javax.swing.GroupLayout jPanelToolsLayout = new javax.swing.GroupLayout(jPanelTools);
        jPanelTools.setLayout(jPanelToolsLayout);
        jPanelToolsLayout.setHorizontalGroup(
                jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelToolsLayout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addGroup(jPanelToolsLayout.createSequentialGroup()
                                                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                                                        .addComponent(jButtonLiveStreams, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                        .addComponent(jButtonAddOld, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                        .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                                                                .addComponent(jButtonAllesSpeichern, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 154, Short.MAX_VALUE)
                                                                .addComponent(btnDiff, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                                        .addComponent(jTextFieldLiveStreams, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 351, Short.MAX_VALUE)
                                                        .addComponent(jTextFieldOld, javax.swing.GroupLayout.Alignment.LEADING)
                                                        .addComponent(txtDiff, javax.swing.GroupLayout.DEFAULT_SIZE, 351, Short.MAX_VALUE))
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                                        .addComponent(jButtonDelLive, javax.swing.GroupLayout.Alignment.TRAILING)
                                                        .addComponent(jButtonOldList)
                                                        .addGroup(jPanelToolsLayout.createSequentialGroup()
                                                                .addComponent(btnPathDiff)
                                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                                                .addComponent(cbkUrl))))
                                        .addGroup(jPanelToolsLayout.createSequentialGroup()
                                                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                                        .addGroup(jPanelToolsLayout.createSequentialGroup()
                                                                .addComponent(btnNurDoppelte)
                                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                                .addComponent(jButtonHashOlddoppelt)
                                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                                .addComponent(jButtonTTUrl))
                                                        .addComponent(btnDelDoppelteUrls, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                        .addGroup(jPanelToolsLayout.createSequentialGroup()
                                                                .addComponent(jButtonFehler)
                                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                                .addComponent(jToggleButtonFastAuto, javax.swing.GroupLayout.PREFERRED_SIZE, 154, javax.swing.GroupLayout.PREFERRED_SIZE)))
                                                .addGap(0, 0, Short.MAX_VALUE)))
                                .addContainerGap())
        );

        jPanelToolsLayout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[]{btnDelDoppelteUrls, btnDiff, btnNurDoppelte, jButtonAllesSpeichern, jButtonFehler, jButtonHashOlddoppelt, jButtonTTUrl, jToggleButtonFastAuto});

        jPanelToolsLayout.setVerticalGroup(
                jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelToolsLayout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jButtonAllesSpeichern)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(jButtonFehler)
                                        .addComponent(jToggleButtonFastAuto))
                                .addGap(18, 18, 18)
                                .addComponent(btnDelDoppelteUrls)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(btnNurDoppelte)
                                        .addComponent(jButtonHashOlddoppelt)
                                        .addComponent(jButtonTTUrl))
                                .addGap(76, 76, 76)
                                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addGroup(jPanelToolsLayout.createSequentialGroup()
                                                .addComponent(jButtonLiveStreams)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                                        .addComponent(jButtonAddOld)
                                                        .addComponent(jTextFieldOld, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                                        .addComponent(jButtonOldList))
                                                .addGap(18, 18, 18)
                                                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                                        .addComponent(btnDiff)
                                                        .addComponent(txtDiff, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                                        .addComponent(btnPathDiff)
                                                        .addComponent(cbkUrl)))
                                        .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                                .addComponent(jTextFieldLiveStreams, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                                .addComponent(jButtonDelLive)))
                                .addContainerGap(102, Short.MAX_VALUE))
        );

        jPanelToolsLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[]{btnPathDiff, txtDiff});

        jPanelToolsLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[]{jButtonAddOld, jTextFieldOld});

        jPanelToolsLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[]{jButtonLiveStreams, jTextFieldLiveStreams});

        jTabbedSender.addTab("Tool", jPanelTools);

        javax.swing.GroupLayout jPanelStartsLayout = new javax.swing.GroupLayout(jPanelStarts);
        jPanelStarts.setLayout(jPanelStartsLayout);
        jPanelStartsLayout.setHorizontalGroup(
                jPanelStartsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 725, Short.MAX_VALUE)
        );
        jPanelStartsLayout.setVerticalGroup(
                jPanelStartsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 419, Short.MAX_VALUE)
        );

        jTabbedSender.addTab("Starts", jPanelStarts);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jTabbedSender)
                                .addContainerGap())
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jTabbedSender)
                                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnDelDoppelteUrls;
    private javax.swing.JButton btnDiff;
    private javax.swing.JButton btnNurDoppelte;
    private javax.swing.JButton btnPathDiff;
    private javax.swing.JCheckBox cbkUrl;
    private javax.swing.JButton jButtonAddOld;
    private javax.swing.JButton jButtonAllesSpeichern;
    private javax.swing.JButton jButtonCheck;
    private javax.swing.JButton jButtonClean;
    private javax.swing.JButton jButtonDelLive;
    private javax.swing.JButton jButtonFehler;
    private javax.swing.JButton jButtonFilmlisteLoeschen;
    private javax.swing.JButton jButtonHashOlddoppelt;
    private javax.swing.JButton jButtonLiveStreams;
    private javax.swing.JButton jButtonNeuLaden;
    private javax.swing.JButton jButtonOldList;
    private javax.swing.JButton jButtonTTUrl;
    private javax.swing.JPanel jPanelFilmlisteLaden;
    private javax.swing.JPanel jPanelLoeschen;
    private javax.swing.JPanel jPanelStarts;
    private javax.swing.JTextField jTextFieldLiveStreams;
    private javax.swing.JTextField jTextFieldOld;
    private javax.swing.JToggleButton jToggleButtonFastAuto;
    private javax.swing.JTextField txtDiff;
    // End of variables declaration//GEN-END:variables

    private class BeobSenderLoeschen implements ActionListener {

        private final String sender;

        public BeobSenderLoeschen(String ssender) {
            sender = ssender;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            daten.getListeFilme().deleteAllFilms(sender);
            daten.getListeBlacklist().filterListe();
            Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, GuiDebug.class.getSimpleName());
        }
    }

    private class BeobPfad implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                FileDialog chooser = new FileDialog(daten.getMediathekGui(), "Pfad");
                chooser.setMode(FileDialog.SAVE);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        File destination = new File(chooser.getDirectory() + chooser.getFile());
                        txtDiff.setText(destination.getAbsolutePath());
                    } catch (Exception ex) {
                        Log.errorLog(679890147, ex);
                    }
                }
            } else {
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                if (!txtDiff.getText().equals("")) {
                    chooser.setCurrentDirectory(new File(txtDiff.getText()));
                }
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setFileHidingEnabled(false);
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        txtDiff.setText(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        Log.errorLog(911025463, ex);
                    }
                }
            }
        }
    }

    private class BeobPfadOldUrl implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                FileDialog chooser = new FileDialog(daten.getMediathekGui(), "Pfad");
                chooser.setMode(FileDialog.SAVE);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        File destination = new File(chooser.getDirectory() + chooser.getFile());
                        jTextFieldOld.setText(destination.getAbsolutePath());
                    } catch (Exception ex) {
                        Log.errorLog(679890147, ex);
                    }
                }
            } else {
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                if (!jTextFieldOld.getText().equals("")) {
                    chooser.setCurrentDirectory(new File(jTextFieldOld.getText()));
                }
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setFileHidingEnabled(false);
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        jTextFieldOld.setText(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        Log.errorLog(911025463, ex);
                    }
                }
            }
        }
    }

}
