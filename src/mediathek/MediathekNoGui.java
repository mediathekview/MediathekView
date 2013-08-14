/*
 *  MediathekView
 *  Copyright (C) 2008 W. Xaver
 *  W.Xaver[at]googlemail.com
 *  http://zdfmediathk.sourceforge.net/
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek;

import java.io.File;
import java.util.Iterator;
import java.util.LinkedList;
import mediathek.controller.filmeLaden.ListenerFilmeLaden;
import mediathek.controller.filmeLaden.ListenerFilmeLadenEvent;
import mediathek.controller.io.IoXmlFilmlisteLesen;
import mediathek.controller.io.IoXmlFilmlisteSchreiben;
import mediathek.daten.Daten;
import mediathek.daten.ListeFilme;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;

public class MediathekNoGui implements Runnable {

    private String output = "";
    private String importUrl = "";
    private String userAgent = "";
    private boolean senderAllesLaden = false;
    private boolean updateFilmliste = true;
    private String pfad = "";
    private Daten daten;
    private boolean serverLaufen = false;
    private File logfile = null;
    private String senderLoeschen = "";

    public MediathekNoGui(String ppfad, boolean ssenderAllesLaden, boolean uupdateFilmliste, String ooutput, String iimprtUrl, String uuserAgent, File log, boolean ddebug) {
        // NUR für den Start vom MediathekServer
        pfad = ppfad;
        senderAllesLaden = ssenderAllesLaden;
        updateFilmliste = uupdateFilmliste;
        output = ooutput;
        importUrl = iimprtUrl;
        userAgent = uuserAgent;
        logfile = log;
        serverLaufen = true;
        if (ddebug) {
            Daten.debug = ddebug;
        }
    }

    public MediathekNoGui(String[] ar) {
        if (ar != null) {
            if (ar.length > 0) {
                if (!ar[0].startsWith("-")) {
                    if (!ar[0].endsWith(File.separator)) {
                        ar[0] += File.separator;
                    }
                    pfad = ar[0];
                }
            }
            for (int i = 0; i < ar.length; ++i) {
                if (ar[i].equals(Main.STARTP_ALLES)) {
                    senderAllesLaden = true;
                    updateFilmliste = false;
                }
                if (ar[i].equalsIgnoreCase(Main.STARTP_EXPORT_DATEI)) {
                    if (ar.length > i) {
                        output = ar[i + 1];
                    }
                }
                if (ar[i].equalsIgnoreCase(Main.STARTP_IMPORT_URL)) {
                    if (ar.length > i) {
                        importUrl = ar[i + 1];
                    }
                }
                if (ar[i].equalsIgnoreCase(Main.STARTP_USER_AGENT)) {
                    if (ar.length > i) {
                        userAgent = ar[i + 1];
                    }
                }
                if (ar[i].equalsIgnoreCase(Main.STARTP_SENDER_LOESCHEN)) {
                    if (ar.length > i) {
                        senderLoeschen = ar[i + 1];
                    }
                }
            }
        }
    }

    public synchronized void init(String[] sender) {
        // für den MediathekServer zum Starten
        daten = new Daten(pfad);
        Daten.nogui = true;
        if (Daten.debug) {
            Log.systemMeldung("Debug on");
        }
        if (!userAgent.equals("")) {
            Daten.setUserAgentManuel(userAgent);
        }
        if (senderAllesLaden) {
            Log.systemMeldung("Filme laden: alles laden");
        } else {
            Log.systemMeldung("Filme laden: nur update laden");
        }
        if (updateFilmliste) {
            Log.systemMeldung("Filmliste: aktualisieren");
        } else {
            Log.systemMeldung("Filmliste: neue erstellen");
        }
        if (logfile != null) {
            Log.setLogFile(logfile);
        }
        Log.systemMeldung("ImportUrl: " + importUrl);
        Log.systemMeldung("Outputfile: " + output);
        Log.systemMeldung("");
        Daten.filmeLaden.addAdListener(new ListenerFilmeLaden() {
            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                serverLaufen = false;
            }
        });
        // laden was es schon gibt
        //Daten.ioXmlFilmlisteLesen.filmlisteLesen(Daten.getBasisVerzeichnis() + Konstanten.XML_DATEI_FILME, false /* istUrl */, Daten.listeFilme);
        new IoXmlFilmlisteLesen().standardFilmlisteLesen();
        // das eigentliche Suchen der Filme bei den Sendern starten
        if (sender == null) {
            Daten.filmeLaden.filmeBeimSenderSuchen(Daten.listeFilme, senderAllesLaden, updateFilmliste);
        } else {
            Daten.filmeLaden.updateSender(sender, Daten.listeFilme, senderAllesLaden);
        }
    }

    @Override
    public synchronized void run() {
        // für den MediathekServer zum Warten aufs Ende
        try {
            while (serverLaufen) {
                this.wait(5000);
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(965451236, Log.FEHLER_ART_NOGUI, MediathekNoGui.class.getName(), "ServerStarten");
        }
        undTschuess(false /* exit */);
    }

    public void stoppen() {
        // für den MediathekServer zum Stroppen/Unerbrechen
        Daten.filmeLaden.setStop();
    }

    public void starten() {
        daten = new Daten(pfad);
        Daten.nogui = true;
        if (!userAgent.equals("")) {
            Daten.setUserAgentManuel(userAgent);
        }
        // Infos schreiben
        Log.startMeldungen(this.getClass().getName());
        if (senderAllesLaden) {
            Log.systemMeldung("Programmstart: alles laden");
        } else {
            Log.systemMeldung("Programmstart: nur update laden");
        }
        Log.systemMeldung("ImportUrl: " + importUrl);
        Log.systemMeldung("Outputfile: " + output);
        Log.systemMeldung("");
        Log.systemMeldung("");
        Daten.filmeLaden.addAdListener(new ListenerFilmeLaden() {
            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                undTschuess(true /* exit */);
            }
        });
        // laden was es schon gibt
        //Daten.ioXmlFilmlisteLesen.filmlisteLesen(Daten.getBasisVerzeichnis() + Konstanten.XML_DATEI_FILME, false /* istUrl */, Daten.listeFilme);
        new IoXmlFilmlisteLesen().standardFilmlisteLesen();
        if (!senderLoeschen.isEmpty()) {
            senderLoeschenUndExit();
        }
        // das eigentliche Suchen der Filme bei den Sendern starten
        Daten.filmeLaden.filmeBeimSenderSuchen(Daten.listeFilme, senderAllesLaden, updateFilmliste);
    }

    private void addImportListe(String url) {
        if (!url.equals("")) {
            Log.systemMeldung("Filmliste importieren von: " + url);
            ListeFilme tmpListe = new ListeFilme();
            new IoXmlFilmlisteLesen().dateiInListeEinlesen(url, GuiFunktionen.istUrl(url) /* istUrl */, tmpListe);
            Daten.listeFilme.updateListe(tmpListe, false /* nur URL vergleichen */);
            tmpListe.clear();
        }
    }

    private void senderLoeschenUndExit() {
        // dann nur einen Sender löschen und dann wieder beenden
        Log.systemMeldung("Sender: " + senderLoeschen + " löschen");
        int anz1 = Daten.listeFilme.size();
        Log.systemMeldung("Anzehl Filme vorher: " + anz1);
        Daten.listeFilme.delSender(senderLoeschen);
        int anz2 = Daten.listeFilme.size();
        Log.systemMeldung("Anzehl Filme nachher: " + anz2);
        Log.systemMeldung(" --> gelöscht: " + (anz1 - anz2));
        new IoXmlFilmlisteSchreiben().filmeSchreiben(Daten.getBasisVerzeichnis(true) + Konstanten.XML_DATEI_FILME, Daten.listeFilme);
        System.exit(0);
    }

    private void undTschuess(boolean exit) {
        if (!importUrl.equals("")) {
            // wenn eine ImportUrl angegeben, dann noch eine Liste importieren
            addImportListe(importUrl);
        }
        new IoXmlFilmlisteSchreiben().filmeSchreiben(Daten.getBasisVerzeichnis(true) + Konstanten.XML_DATEI_FILME, Daten.listeFilme);
        if (!output.equals("")) {
            LinkedList<String> out = new LinkedList<String>();
            String tmp;
            do {
                if (output.startsWith(",")) {
                    output = output.substring(1);
                }
                if (output.contains(",")) {
                    tmp = output.substring(0, output.indexOf(","));
                    output = output.substring(output.indexOf(","));
                    out.add(tmp);
                } else {
                    out.add(output);
                }
            } while (output.contains(","));
            Iterator<String> it = out.iterator();
            while (it.hasNext()) {
                //datei schreiben
                new IoXmlFilmlisteSchreiben().filmeSchreiben(it.next(), Daten.listeFilme);
            }
        }
        Log.printEndeMeldung();
        if (exit) {
            // nur dann das Programm beenden
            if (Daten.listeFilme.isEmpty()) {
                //Satz mit x, war wohl nix
                System.exit(1);
            } else {
                System.exit(0);
            }
        }
    }
}
