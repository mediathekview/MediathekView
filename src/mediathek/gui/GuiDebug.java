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

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URL;
import java.net.URLConnection;
import java.text.SimpleDateFormat;
import java.util.Date;
import javax.swing.JButton;
import javax.swing.JLabel;
import mediathek.MediathekGui;
import mediathek.daten.Daten;
import mediathek.gui.dialogEinstellungen.PanelListeFilmlistenServer;
import mediathek.gui.dialogEinstellungen.PanelSenderLaden;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;
import mediathek.tool.MVNotification;
import msearch.daten.MSearchConfig;
import msearch.io.MSearchFilmlisteLesen;
import msearch.io.MSearchFilmlisteSchreiben;

public class GuiDebug extends PanelVorlage {

    private JButton[] buttonSender;
    private String[] sender;

    public GuiDebug(Daten d, Frame parentComponent) {
        super(d, parentComponent);
        initComponents();
        daten = d;
        sender = Daten.filmeLaden.getSenderNamen();
        buttonSender = new JButton[sender.length];
        for (int i = 0; i < Daten.filmeLaden.getSenderNamen().length; ++i) {
            buttonSender[i] = new JButton(sender[i]);
            buttonSender[i].addActionListener(new BeobSenderLoeschen(sender[i]));
        }
        addSender();
        jToggleButtonAllesLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                MSearchConfig.senderAllesLaden = jToggleButtonAllesLaden.isSelected();
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
        jButtonAlleLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        Daten.filmeLaden.filmeBeimSenderSuchen(Daten.listeFilme, jToggleButtonAllesLaden.isSelected(), true);
                    }
                }).start();
            }
        });
        jCheckBoxDateiGroesse.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                //Daten.STOP_DATEIGROESSE = jCheckBoxDateiGroesse.isSelected();
            }
        });
        jPanelSenderLaden.setLayout(new BorderLayout());
        jPanelSenderLaden.add(new PanelSenderLaden(daten, daten.mediathekGui));
        jPanelListen.setLayout(new BorderLayout());
        jPanelListen.add(new PanelListeFilmlistenServer(d, daten.mediathekGui));
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
        jButtonDoppelt.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.listeFilme.nurDoppelteAnzeigen(false);
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, MediathekGui.class.getSimpleName());
            }
        });
        jButtonDoppeltIndex.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.listeFilme.nurDoppelteAnzeigen(true);
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, MediathekGui.class.getSimpleName());
            }
        });
        jButtonGc.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                System.gc();
            }
        });
        jButtonNotify.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                MVNotification.addNotification("Titel", "<html>"
                        + "<head>"
                        + "</head>"
                        + "<body>"
                        + "<p>"
                        + "<b>Autoren:</b><br />"
                        + "Xaver W. (W.Xaver [at] googlemail [dot] com)<br />"
                        + "Christian F. (crystalpalace1977 [at] googlemail [dot] com)"
                        + "</p>"
                        + "</body>"
                        + "</html>");
            }
        });
        jButtonListeSchreiben.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Date startZeit = null;
                Date stopZeit = null;
                startZeit = new Date(System.currentTimeMillis());

                new MSearchFilmlisteSchreiben().filmlisteSchreibenXml(Daten.getDateiFilmliste(), Daten.listeFilme);

                stopZeit = new Date(System.currentTimeMillis());
                SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
                int sekunden;
                try {
                    sekunden = Math.round(stopZeit.getTime() - startZeit.getTime());
                } catch (Exception ex) {
                    sekunden = -1;
                }
                System.out.println("======================================");
                System.out.println("     ->    Dauer[ms]: " + sekunden);
                System.out.println("======================================");
            }
        });
        jButtonListeLesen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Date startZeit = null;
                Date stopZeit = null;
                Daten.listeFilme.clear();
                startZeit = new Date(System.currentTimeMillis());

                new MSearchFilmlisteLesen().filmlisteLesenXml(Daten.getDateiFilmliste(), Daten.listeFilme);

                stopZeit = new Date(System.currentTimeMillis());
                SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
                int sekunden;
                try {
                    sekunden = Math.round(stopZeit.getTime() - startZeit.getTime());
                } catch (Exception ex) {
                    sekunden = -1;
                }
                System.out.println("======================================");
                System.out.println("     ->    Dauer[ms]: " + sekunden);
                System.out.println("======================================");
                daten.listeAbo.aenderungMelden();

            }
        });
        jButtonJsonSchreiben.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Date startZeit = null;
                Date stopZeit = null;
                SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
                int sekunden;
                startZeit = new Date(System.currentTimeMillis());
                new MSearchFilmlisteSchreiben().filmlisteSchreibenJson(Daten.getDateiFilmliste() + ".json", Daten.listeFilme);
                stopZeit = new Date(System.currentTimeMillis());
                try {
                    sekunden = Math.round(stopZeit.getTime() - startZeit.getTime());
                } catch (Exception ex) {
                    sekunden = -1;
                }
                System.out.println("======================================");
                System.out.println(" normal");
                System.out.println("     ->    Dauer[ms]: " + sekunden);
                System.out.println("======================================");
                startZeit = new Date(System.currentTimeMillis());
                new MSearchFilmlisteSchreiben().filmlisteSchreibenJson(Daten.getDateiFilmliste() + "_json.bz2", Daten.listeFilme);
                stopZeit = new Date(System.currentTimeMillis());
                try {
                    sekunden = Math.round(stopZeit.getTime() - startZeit.getTime());
                } catch (Exception ex) {
                    sekunden = -1;
                }
                System.out.println("======================================");
                System.out.println(" bz2 ");
                System.out.println("     ->    Dauer[ms]: " + sekunden);
                System.out.println("======================================");
            }
        });
        jButtonjSonLesen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Date startZeit = null;
                Date stopZeit = null;
                SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
                int sekunden;

                Daten.listeFilme.clear();
                startZeit = new Date(System.currentTimeMillis());
                //http://176.28.8.161/mediathek1/filme_json.zip
                //new MSearchFilmlisteLesen().filmlisteLesenJson(Daten.getDateiFilmliste() + "_json", "", Daten.listeFilme);
                new MSearchFilmlisteLesen().filmlisteLesenJson("http://176.28.8.161/mediathek1/filme_json.bz2", Daten.getDateiFilmliste() + "_json---", Daten.listeFilme);
                stopZeit = new Date(System.currentTimeMillis());
                try {
                    sekunden = Math.round(stopZeit.getTime() - startZeit.getTime());
                } catch (Exception ex) {
                    sekunden = -1;
                }
                System.out.println("======================================");
                System.out.println(" web ");
                System.out.println("     ->    Dauer[ms]: " + sekunden);
                System.out.println("======================================");

                Daten.listeFilme.clear();
                startZeit = new Date(System.currentTimeMillis());
                new MSearchFilmlisteLesen().filmlisteLesenJson(Daten.getDateiFilmliste() + ".json", Daten.getDateiFilmliste() + "_json---", Daten.listeFilme);
                stopZeit = new Date(System.currentTimeMillis());
                try {
                    sekunden = Math.round(stopZeit.getTime() - startZeit.getTime());
                } catch (Exception ex) {
                    sekunden = -1;
                }
                System.out.println("======================================");
                System.out.println(" normal ");
                System.out.println("     ->    Dauer[ms]: " + sekunden);
                System.out.println("======================================");

                Daten.listeFilme.clear();
                startZeit = new Date(System.currentTimeMillis());
                new MSearchFilmlisteLesen().filmlisteLesenJson(Daten.getDateiFilmliste() + "_json.bz2", Daten.getDateiFilmliste() + "_json---", Daten.listeFilme);
                stopZeit = new Date(System.currentTimeMillis());
                try {
                    sekunden = Math.round(stopZeit.getTime() - startZeit.getTime());
                } catch (Exception ex) {
                    sekunden = -1;
                }
                System.out.println("======================================");
                System.out.println(" bz2 ");
                System.out.println("     ->    Dauer[ms]: " + sekunden);
                System.out.println("======================================");
                daten.listeAbo.aenderungMelden();

            }
        });
    }

    private void addSender() {
        jPanelSender.removeAll();
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.weightx = 0;
        jPanelSender.setLayout(gridbag);
        c.gridx = 0;
        c.gridy = 0;
        c.anchor = GridBagConstraints.WEST;
        c.fill = GridBagConstraints.BOTH;
        int nr = 0;
        int y = 0;
        int halbe = sender.length / 2;
        halbe += sender.length % 2;
        for (String aSender : sender) {
            c.gridy = y;
            addPanel(gridbag, c, aSender, nr);
            ++nr;
            ++y;
            if (y >= halbe) {
                y = 0;
                c.gridx = 1;
            }
        }
        JLabel label = new JLabel();
        c.gridx = 4;
        c.weightx = 2;
        gridbag.setConstraints(label, c);
        jPanelSender.add(label);
        jPanelSender.updateUI();
    }

    private void addPanel(GridBagLayout gridbag, GridBagConstraints c, String sender, int nr) {
        c.insets = new Insets(2, 10, 2, 2);
        gridbag.setConstraints(buttonSender[nr], c);
        jPanelSender.add(buttonSender[nr]);
    }
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JTabbedPane jTabbedPane1 = new javax.swing.JTabbedPane();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jPanelSenderLaden = new javax.swing.JPanel();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        jPanelSender = new javax.swing.JPanel();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        jPanelListen = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        jButtonCheckUrl = new javax.swing.JButton();
        jTextFieldUrl = new javax.swing.JTextField();
        javax.swing.JPanel jPanel4 = new javax.swing.JPanel();
        jToggleButtonAllesLaden = new javax.swing.JToggleButton();
        jButtonFilmlisteLoeschen = new javax.swing.JButton();
        jButtonAllesSpeichern = new javax.swing.JButton();
        jButtonFehler = new javax.swing.JButton();
        jButtonCheck = new javax.swing.JButton();
        jButtonAlleLaden = new javax.swing.JButton();
        jCheckBoxDateiGroesse = new javax.swing.JCheckBox();
        jButtonDoppelt = new javax.swing.JButton();
        jButtonDoppeltIndex = new javax.swing.JButton();
        jButtonGc = new javax.swing.JButton();
        jButtonNotify = new javax.swing.JButton();
        jButtonListeSchreiben = new javax.swing.JButton();
        jButtonListeLesen = new javax.swing.JButton();
        jButtonJsonSchreiben = new javax.swing.JButton();
        jButtonjSonLesen = new javax.swing.JButton();

        jPanelSenderLaden.setBorder(javax.swing.BorderFactory.createTitledBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED), "Sender starten"));

        javax.swing.GroupLayout jPanelSenderLadenLayout = new javax.swing.GroupLayout(jPanelSenderLaden);
        jPanelSenderLaden.setLayout(jPanelSenderLadenLayout);
        jPanelSenderLadenLayout.setHorizontalGroup(
            jPanelSenderLadenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 823, Short.MAX_VALUE)
        );
        jPanelSenderLadenLayout.setVerticalGroup(
            jPanelSenderLadenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 444, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelSenderLaden, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelSenderLaden, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("tab2", jPanel2);

        jPanelSender.setBorder(javax.swing.BorderFactory.createTitledBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED), "Sender löschen"));

        javax.swing.GroupLayout jPanelSenderLayout = new javax.swing.GroupLayout(jPanelSender);
        jPanelSender.setLayout(jPanelSenderLayout);
        jPanelSenderLayout.setHorizontalGroup(
            jPanelSenderLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 823, Short.MAX_VALUE)
        );
        jPanelSenderLayout.setVerticalGroup(
            jPanelSenderLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 444, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelSender, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelSender, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("tab1", jPanel1);

        javax.swing.GroupLayout jPanelListenLayout = new javax.swing.GroupLayout(jPanelListen);
        jPanelListen.setLayout(jPanelListenLayout);
        jPanelListenLayout.setHorizontalGroup(
            jPanelListenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 837, Short.MAX_VALUE)
        );
        jPanelListenLayout.setVerticalGroup(
            jPanelListenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 468, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelListen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelListen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("tab3", jPanel3);

        jButtonCheckUrl.setText("check URL");

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jButtonCheckUrl)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jTextFieldUrl, javax.swing.GroupLayout.DEFAULT_SIZE, 726, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonCheckUrl)
                    .addComponent(jTextFieldUrl, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(455, Short.MAX_VALUE))
        );

        jPanel5Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonCheckUrl, jTextFieldUrl});

        jTabbedPane1.addTab("tab4", jPanel5);

        jPanel4.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jToggleButtonAllesLaden.setText("[-alles] setzen");

        jButtonFilmlisteLoeschen.setText("Filmliste löschen");

        jButtonAllesSpeichern.setText("alles speichern");

        jButtonFehler.setText("Fehler ausgeben");

        jButtonCheck.setText("Check");

        jButtonAlleLaden.setText("alle S. laden");

        jCheckBoxDateiGroesse.setText("Stop Dateigröße");
        jCheckBoxDateiGroesse.setEnabled(false);

        jButtonDoppelt.setText("nur doppelte URLs");

        jButtonDoppeltIndex.setText("nur doppelte URLs, index");

        jButtonGc.setText("Gc");

        jButtonNotify.setText("Notify");

        jButtonListeSchreiben.setText("ListeSchreiben");

        jButtonListeLesen.setText("ListeLesen");

        jButtonJsonSchreiben.setText("jSon schreiben");

        jButtonjSonLesen.setText("jSon lesen");

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(jCheckBoxDateiGroesse)
                        .addGap(18, 18, 18)
                        .addComponent(jButtonDoppelt)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonDoppeltIndex)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonGc)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonNotify))
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(jToggleButtonAllesLaden)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonFilmlisteLoeschen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonAllesSpeichern)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonFehler)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonCheck)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonAlleLaden))
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(jButtonListeSchreiben)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonListeLesen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonJsonSchreiben)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonjSonLesen)))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel4Layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonAllesSpeichern, jButtonFilmlisteLoeschen, jToggleButtonAllesLaden});

        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jToggleButtonAllesLaden)
                    .addComponent(jButtonFilmlisteLoeschen)
                    .addComponent(jButtonAllesSpeichern)
                    .addComponent(jButtonFehler)
                    .addComponent(jButtonCheck)
                    .addComponent(jButtonAlleLaden))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jCheckBoxDateiGroesse)
                    .addComponent(jButtonDoppelt)
                    .addComponent(jButtonDoppeltIndex)
                    .addComponent(jButtonGc)
                    .addComponent(jButtonNotify))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonListeSchreiben)
                    .addComponent(jButtonListeLesen)
                    .addComponent(jButtonJsonSchreiben)
                    .addComponent(jButtonjSonLesen))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jTabbedPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jTabbedPane1)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAlleLaden;
    private javax.swing.JButton jButtonAllesSpeichern;
    private javax.swing.JButton jButtonCheck;
    private javax.swing.JButton jButtonCheckUrl;
    private javax.swing.JButton jButtonDoppelt;
    private javax.swing.JButton jButtonDoppeltIndex;
    private javax.swing.JButton jButtonFehler;
    private javax.swing.JButton jButtonFilmlisteLoeschen;
    private javax.swing.JButton jButtonGc;
    private javax.swing.JButton jButtonJsonSchreiben;
    private javax.swing.JButton jButtonListeLesen;
    private javax.swing.JButton jButtonListeSchreiben;
    private javax.swing.JButton jButtonNotify;
    private javax.swing.JButton jButtonjSonLesen;
    private javax.swing.JCheckBox jCheckBoxDateiGroesse;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanelListen;
    private javax.swing.JPanel jPanelSender;
    private javax.swing.JPanel jPanelSenderLaden;
    private javax.swing.JTextField jTextFieldUrl;
    private javax.swing.JToggleButton jToggleButtonAllesLaden;
    // End of variables declaration//GEN-END:variables

    private class BeobSenderLoeschen implements ActionListener {

        private String sender;

        public BeobSenderLoeschen(String ssender) {
            sender = ssender;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            Daten.listeFilme.delSender(sender);
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_FILMLISTE_GEAENDERT, MediathekGui.class.getSimpleName());
        }
    }
}
