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
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import mediathek.MediathekGui;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.gui.dialogEinstellungen.PanelFilmlisten;
import mediathek.tool.Duration;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import msearch.daten.DatenFilm;
import msearch.daten.ListeFilme;
import msearch.filmlisten.MSFilmlisteLesen;
import msearch.tool.MSConst;

public class GuiDebug extends JPanel {

    private final JButton[] buttonSender;
    private final String[] sender;
    private Daten daten;

    public GuiDebug(Daten d) {
        super();
        initComponents();
        daten = d;
        sender = Daten.filmeLaden.getSenderNamen();
        buttonSender = new JButton[sender.length];

        jPanelFilmlisteLaden.setLayout(new GridLayout(1, 1));
        jPanelFilmlisteLaden.add(new PanelFilmlisten(d, daten.mediathekGui));

        jPanelStarts.setLayout(new GridLayout(1, 1));
        jPanelStarts.add(new PanelInfoStarts());

        //Tab1 Sender löschen Panel füllen
        for (int i = 0; i < Daten.filmeLaden.getSenderNamen().length; ++i) {
            buttonSender[i] = new JButton(sender[i]);
            buttonSender[i].addActionListener(new BeobSenderLoeschen(sender[i]));
        }
        addSender();
        jButtonNeuLaden.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent ae) {
                Daten.listeFilme.clear();
                Duration duration = new Duration(MediathekGui.class.getSimpleName());
                duration.ping("Start");
                new MSFilmlisteLesen().readFilmListe(Daten.getDateiFilmliste(), Daten.listeFilme, Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE)));
//                    new FilmListReader().readFilmListe(new URI("http://www.wp11128329.server-he.de/filme/Filmliste-akt.xz"), Daten.listeFilme);
                duration.ping("Fertig");
                Daten.listeFilme.themenLaden();
                Daten.listeAbo.setAboFuerFilm(Daten.listeFilme, false /*aboLoeschen*/);
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, MediathekGui.class.getSimpleName());
            }
        });
        jButtonAllesSpeichern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.allesSpeichern();
                Daten.filmlisteSpeichern();
            }
        });
        jButtonFilmlisteLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.listeFilme.clear();
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, MediathekGui.class.getSimpleName());
            }
        });
        jButtonFehler.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Log.printEndeMeldung();
            }
        });
        jButtonCheck.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.listeFilme.check();
            }
        });

        jButtonCheckUrl.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                long l = 0;
                try {
                    URLConnection co = new URL(jTextFieldUrl.getText()).openConnection();
                    l = co.getContentLengthLong();
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
                System.out.println("Byte: " + l);
            }
        });
        jButtonGc.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                System.gc();
            }
        });
        jButtonClean.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent ae) {
                Daten.listeFilme.cleanList();
            }
        });
        jToggleButtonFastAuto.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent ae) {
                if (jToggleButtonFastAuto.isSelected()) {
                    MSFilmlisteLesen.setWorkMode(MSFilmlisteLesen.WorkMode.FASTAUTO);
                } else {
                    MSFilmlisteLesen.setWorkMode(MSFilmlisteLesen.WorkMode.NORMAL);
                }
            }
        });
        jButtonDir.addActionListener(new BeobPfad());
        jButtonSize.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                String path = jTextFieldPath.getText();
                try {
                    System.out.println("");
                    System.out.println("=============================");
                    File file = new File(path);
                    long i = file.getFreeSpace();
                    System.out.println("getFreeSpace " + i);
                    System.out.println(" " + i / 1024 / 1024 / 1024);
                    i = file.getTotalSpace();
                    System.out.println("=============================");
                    System.out.println("getTotalSpace " + i);
                    System.out.println(" " + i / 1024 / 1024 / 1024);
                    i = file.getUsableSpace();
                    System.out.println("=============================");
                    System.out.println("getUsableSpace " + i);
                    System.out.println(" " + i / 1024 / 1024 / 1024);
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
            }
        });
        jButtonSearchUrl.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                if (!jTextFieldSearchUrl.getText().isEmpty()) {
                    daten.guiFilme.searchUrl(jTextFieldSearchUrl.getText());
                }
            }
        });
        addComponentListener(new java.awt.event.ComponentAdapter() {
            @Override
            public void componentShown(java.awt.event.ComponentEvent evt) {
                daten.mediathekGui.setToolbar(MVToolBar.TOOLBAR_NIX);
                daten.mediathekGui.getStatusBar().setIndexForLeftDisplay(MVStatusBar.StatusbarIndex.FILME);
            }
        });
        jButtonGetName.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
//                int count = 0;
//                OutputStreamWriter out = null;
//                ArrayList<String> list = new ArrayList<>();
//                ArrayList<String> listUrl = new ArrayList<>();
//                ListeFilme listeFilme = new ListeFilme();
//                try {
//                    System.out.println("---------------------");
//                    File file = new File("/tmp/name");
//                    out = new OutputStreamWriter(new FileOutputStream(file), MSConst.KODIERUNG_UTF);
//                    for (DatenFilm film : Daten.listeFilme) {
//                        if (!listUrl.contains(film.arr[DatenFilm.FILM_URL_NR])) {
//                            listeFilme.add(film);
//                            listUrl.add(film.arr[DatenFilm.FILM_URL_NR]);
//                        }
//                    }
//
//                    for (DatenFilm film : listeFilme) {
//                        String s = GuiFunktionen.getDateiName(film.arr[DatenFilm.FILM_URL_NR]);
//                        String te = film.arr[DatenFilm.FILM_THEMA_NR];
//                        String ti = film.arr[DatenFilm.FILM_TITEL_NR];
//                        list.add(te + "--" + ti + "--" + s);
//                    }
//                    Collections.sort(list);
//                    String old = "";
//                    for (String s : list) {
//                        if (old.equals(s)) {
//                            ++count;
//                            out.write("\n\n\n");
//                            out.write("============ doppelt ================\n");
//                            out.write(s + "\n");
//                            out.write("============================\n");
//                        } else {
////                            out.write(s + "\n");
//                        }
//                        old = s;
//                    }
//                    out.write("\n");
//                    out.write("\n");
//                    out.write("===============================" + "\n");
//                    out.write("Doppelte: " + count + "\n");
//                } catch (Exception ex) {
//                } finally {
//                    try {
//                        out.close();
//                    } catch (IOException ex) {
//                        Logger.getLogger(GuiDebug.class.getName()).log(Level.SEVERE, null, ex);
//                    }
//                }

                int count = 0;
                OutputStreamWriter out = null;
                ArrayList<String> list = new ArrayList<>();
                try {
                    System.out.println("---------------------");
                    File file = new File("/tmp/name");
                    out = new OutputStreamWriter(new FileOutputStream(file), MSConst.KODIERUNG_UTF);

                    for (DatenFilm film : Daten.listeFilme) {
//                        if ((film.arr[DatenFilm.FILM_URL_NR].hashCode() + "").equals("1183061251")) {
//                            System.out.println(film.arr[DatenFilm.FILM_URL_NR]);
//                        }
//                        if ((film.arr[DatenFilm.FILM_URL_NR].hashCode() + "").equals("-1183061251")) {
//                            System.out.println(film.arr[DatenFilm.FILM_URL_NR]);
//                        }

                        String s = GuiFunktionen.getDateiName(film.arr[DatenFilm.FILM_URL_NR]);
                        String h = GuiFunktionen.getHash(film.arr[DatenFilm.FILM_URL_NR]);
                        String se = film.arr[DatenFilm.FILM_SENDER_NR];
                        String te = film.arr[DatenFilm.FILM_THEMA_NR];
                        String ti = film.arr[DatenFilm.FILM_TITEL_NR];
                        list.add(h);
//                        list.add(se + "--" + te + "--" + ti + "------" + s);
//                        list.add(te + "--" + ti + "------" + s);
                    }
                    Collections.sort(list);
                    String old = "";
                    for (String s : list) {
                        if (old.equals(s)) {
                            ++count;
                            out.write("===============================" + "\n");
                            out.write(s + "\n");
                        } else {
                            out.write(s + "\n");
                        }
                        old = s;
                    }
                    out.write("\n");
                    out.write("\n");
                    out.write("===============================" + "\n");
                    out.write("Doppelte: " + count + "\n");
                } catch (Exception ex) {
                } finally {
                    try {
                        out.close();
                    } catch (IOException ex) {
                        Logger.getLogger(GuiDebug.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }

            }
        });
        jButtonDoppelteUrls.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                ListeFilme listeFilme = new ListeFilme();
                ArrayList<String> listUrl = new ArrayList<>();
                for (DatenFilm film : Daten.listeFilme) {
                    if (!listUrl.contains(film.arr[DatenFilm.FILM_URL_NR])) {
                        listeFilme.add(film);
                        listUrl.add(film.arr[DatenFilm.FILM_URL_NR]);
                    }
                }
                System.out.println("---------------------");
                System.out.println("vorher: " + Daten.listeFilme.size());
                Daten.listeFilme = listeFilme;
                System.out.println("danach: " + Daten.listeFilme.size());
                Daten.filmlisteSpeichern();
            }
        });
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
        jButtonCheckUrl = new javax.swing.JButton();
        jTextFieldUrl = new javax.swing.JTextField();
        jButtonGc = new javax.swing.JButton();
        jButtonFehler = new javax.swing.JButton();
        jButtonAllesSpeichern = new javax.swing.JButton();
        jToggleButtonFastAuto = new javax.swing.JToggleButton();
        jButtonSize = new javax.swing.JButton();
        jTextFieldPath = new javax.swing.JTextField();
        jButtonDir = new javax.swing.JButton();
        jButtonSearchUrl = new javax.swing.JButton();
        jTextFieldSearchUrl = new javax.swing.JTextField();
        jButtonGetName = new javax.swing.JButton();
        jButtonDoppelteUrls = new javax.swing.JButton();
        jPanelStarts = new javax.swing.JPanel();

        javax.swing.GroupLayout jPanelFilmlisteLadenLayout = new javax.swing.GroupLayout(jPanelFilmlisteLaden);
        jPanelFilmlisteLaden.setLayout(jPanelFilmlisteLadenLayout);
        jPanelFilmlisteLadenLayout.setHorizontalGroup(
            jPanelFilmlisteLadenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 702, Short.MAX_VALUE)
        );
        jPanelFilmlisteLadenLayout.setVerticalGroup(
            jPanelFilmlisteLadenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 489, Short.MAX_VALUE)
        );

        jTabbedSender.addTab("Filmliste laden", jPanelFilmlisteLaden);

        jPanelSender.setBorder(javax.swing.BorderFactory.createTitledBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED), "Sender löschen"));

        javax.swing.GroupLayout jPanelLoeschenLayout = new javax.swing.GroupLayout(jPanelLoeschen);
        jPanelLoeschen.setLayout(jPanelLoeschenLayout);
        jPanelLoeschenLayout.setHorizontalGroup(
            jPanelLoeschenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 35, Short.MAX_VALUE)
        );
        jPanelLoeschenLayout.setVerticalGroup(
            jPanelLoeschenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 111, Short.MAX_VALUE)
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
                .addContainerGap(240, Short.MAX_VALUE))
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
                        .addGap(0, 134, Short.MAX_VALUE)))
                .addContainerGap())
        );

        jPanelFilmlisteLayout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonCheck, jButtonFilmlisteLoeschen, jButtonNeuLaden});

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
                .addComponent(jPanelSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        jTabbedSender.addTab("Filmliste", jPanelFilmliste);

        jButtonCheckUrl.setText("get URL Filesize:");

        jButtonGc.setText("Gc");

        jButtonFehler.setText("Fehler ausgeben");

        jButtonAllesSpeichern.setText("alles speichern");

        jToggleButtonFastAuto.setText("-FASTAUTO");

        jButtonSize.setText("getTotalSpace");

        jTextFieldPath.setText("/");

        jButtonDir.setText(":::");

        jButtonSearchUrl.setText("URL suchen");

        jButtonGetName.setText("getName");

        jButtonDoppelteUrls.setText("doppelte URLs löschen");

        javax.swing.GroupLayout jPanelToolsLayout = new javax.swing.GroupLayout(jPanelTools);
        jPanelTools.setLayout(jPanelToolsLayout);
        jPanelToolsLayout.setHorizontalGroup(
            jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelToolsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanelToolsLayout.createSequentialGroup()
                        .addComponent(jButtonCheckUrl)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldUrl))
                    .addGroup(jPanelToolsLayout.createSequentialGroup()
                        .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                            .addComponent(jButtonGetName, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jButtonFehler, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jButtonGc, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jButtonAllesSpeichern, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jToggleButtonFastAuto, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jButtonSize, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jButtonSearchUrl, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanelToolsLayout.createSequentialGroup()
                                .addComponent(jTextFieldPath, javax.swing.GroupLayout.DEFAULT_SIZE, 463, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonDir))
                            .addComponent(jTextFieldSearchUrl)))
                    .addGroup(jPanelToolsLayout.createSequentialGroup()
                        .addComponent(jButtonDoppelteUrls)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanelToolsLayout.setVerticalGroup(
            jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelToolsLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jButtonAllesSpeichern)
                .addGap(18, 18, 18)
                .addComponent(jButtonGc)
                .addGap(18, 18, 18)
                .addComponent(jButtonFehler)
                .addGap(18, 18, 18)
                .addComponent(jToggleButtonFastAuto)
                .addGap(65, 65, 65)
                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonSize)
                    .addComponent(jTextFieldPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonDir))
                .addGap(18, 18, 18)
                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonSearchUrl)
                    .addComponent(jTextFieldSearchUrl, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addComponent(jButtonGetName)
                .addGap(18, 18, 18)
                .addComponent(jButtonDoppelteUrls)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 67, Short.MAX_VALUE)
                .addGroup(jPanelToolsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonCheckUrl)
                    .addComponent(jTextFieldUrl, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        jPanelToolsLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonCheckUrl, jTextFieldUrl});

        jPanelToolsLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonDir, jTextFieldPath});

        jPanelToolsLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonSearchUrl, jTextFieldSearchUrl});

        jTabbedSender.addTab("Tool", jPanelTools);

        javax.swing.GroupLayout jPanelStartsLayout = new javax.swing.GroupLayout(jPanelStarts);
        jPanelStarts.setLayout(jPanelStartsLayout);
        jPanelStartsLayout.setHorizontalGroup(
            jPanelStartsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 702, Short.MAX_VALUE)
        );
        jPanelStartsLayout.setVerticalGroup(
            jPanelStartsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 485, Short.MAX_VALUE)
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
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTabbedSender)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAllesSpeichern;
    private javax.swing.JButton jButtonCheck;
    private javax.swing.JButton jButtonCheckUrl;
    private javax.swing.JButton jButtonClean;
    private javax.swing.JButton jButtonDir;
    private javax.swing.JButton jButtonDoppelteUrls;
    private javax.swing.JButton jButtonFehler;
    private javax.swing.JButton jButtonFilmlisteLoeschen;
    private javax.swing.JButton jButtonGc;
    private javax.swing.JButton jButtonGetName;
    private javax.swing.JButton jButtonNeuLaden;
    private javax.swing.JButton jButtonSearchUrl;
    private javax.swing.JButton jButtonSize;
    private javax.swing.JPanel jPanelFilmlisteLaden;
    private javax.swing.JPanel jPanelLoeschen;
    private javax.swing.JPanel jPanelStarts;
    private javax.swing.JTextField jTextFieldPath;
    private javax.swing.JTextField jTextFieldSearchUrl;
    private javax.swing.JTextField jTextFieldUrl;
    private javax.swing.JToggleButton jToggleButtonFastAuto;
    // End of variables declaration//GEN-END:variables

    private class BeobSenderLoeschen implements ActionListener {

        private final String sender;

        public BeobSenderLoeschen(String ssender) {
            sender = ssender;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            Daten.listeFilme.deleteAllFilms(sender);
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, MediathekGui.class.getSimpleName());
        }
    }

    private class BeobPfad implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                FileDialog chooser = new FileDialog(daten.mediathekGui, "Pfad");
                chooser.setMode(FileDialog.SAVE);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        File destination = new File(chooser.getDirectory() + chooser.getFile());
                        jTextFieldPath.setText(destination.getAbsolutePath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(679890147, ex);
                    }
                }
            } else {
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                if (!jTextFieldPath.getText().equals("")) {
                    chooser.setCurrentDirectory(new File(jTextFieldPath.getText()));
                }
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setFileHidingEnabled(false);
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        jTextFieldPath.setText(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(911025463, ex);
                    }
                }
            }
        }
    }
}
