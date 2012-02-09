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
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import mediathek.controller.filme.BeobFilmeLaden;
import mediathek.controller.filme.FilmListenerElement;
import mediathek.controller.filme.FilmeLaden;
import mediathek.controller.filme.filmeExportieren.FilmeExportieren;
import mediathek.controller.io.IoXmlFilmlisteLesen;
import mediathek.controller.io.IoXmlFilmlisteSchreiben;
import mediathek.daten.ListeFilme;

public class MediathekNoGui {

    public final String STARTP_ALLES = "-alles";
    public final String STARTP_USER_AGENT = "-agent";
    public final String STARTP_EXPORT_DATEI = "-o";
    public final String STARTP_IMPORT_URL = "-i";
    private String output = "";
    private String importUrl = "";
    private String userAgent = "";
    private boolean allesLaden = false;
    private Date startZeit = new Date(System.currentTimeMillis());
    private Date stopZeit = null;
    private ListeFilme listeFilme = null;
    private FilmeLaden filmeLaden = null;
    private String pfad = "";
    private IoXmlFilmlisteSchreiben ioXmlFilmlisteSchreiben = null;
    private IoXmlFilmlisteLesen ioXmlFilmlisteLesen = null;

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
                if (ar[i].equals(STARTP_ALLES)) {
                    allesLaden = true;
                }
                if (ar[i].equalsIgnoreCase(STARTP_EXPORT_DATEI)) {
                    if (ar.length > i) {
                        output = ar[i + 1];
                    }
                }
                if (ar[i].equalsIgnoreCase(STARTP_IMPORT_URL)) {
                    if (ar.length > i) {
                        importUrl = ar[i + 1];
                    }
                }
                if (ar[i].equalsIgnoreCase(STARTP_USER_AGENT)) {
                    if (ar.length > i) {
                        userAgent = ar[i + 1];
                    }
                }
            }
        }
        if (allesLaden) {
            Log.systemMeldung("Programmstart: alles laden");
        } else {
            Log.systemMeldung("Programmstart: nur update laden");
        }
    }

    public void starten() {
        // initialisieren
        ioXmlFilmlisteSchreiben = new IoXmlFilmlisteSchreiben();
        ioXmlFilmlisteLesen = new IoXmlFilmlisteLesen();
        filmeLaden = new FilmeLaden();
        if (!userAgent.equals("")) {
            Daten.setUserAgentManuel(userAgent);
        }
        filmeLaden.setAllesLaden(allesLaden);
        listeFilme = new ListeFilme();
        filmeLaden.addAdListener(new BeobachterLadenFilme());
        // laden was es schon gibt
        filmeLaden.filmlisteImportieren(pfad);
        addImportListe(importUrl);
    }

    private void addImportListe(String url) {
        if (!url.equals("")) {
            Log.systemMeldung("Filmliste importieren von: " + url);
            ListeFilme tmpListe = new ListeFilme();
            ioXmlFilmlisteLesen.filmlisteLesen(url, false /* istDatei */, tmpListe, Daten.getUserAgent());
            listeFilme.updateListe(tmpListe, false /* nur URL vergleichen */);
            tmpListe.clear();
        }
    }

    private void undTschuess() {
        listeFilme = filmeLaden.getListeFilme();
        new FilmeExportieren().filmeSchreiben(listeFilme);
        if (!output.equals("")) {
            LinkedList<String> out = new LinkedList<String>();
            String tmp = "";
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
                ioXmlFilmlisteSchreiben.filmeSchreiben(it.next(), listeFilme);
            }
        }
        stopZeit = new Date(System.currentTimeMillis());
        SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
        int minuten = 0;
        try {
            minuten = Math.round((stopZeit.getTime() - startZeit.getTime()) / (1000 * 60));
        } catch (Exception ex) {
            minuten = -1;
        }
        Log.systemMeldung(new String[]{
                    "========================================",
                    "  " + filmeLaden.getSeitenGeladen() + " Seiten geladen",
                    "  " + listeFilme.size() + " Filme gesamt",
                    "  --> Beginn: " + sdf.format(startZeit),
                    "  --> Fertig: " + sdf.format(stopZeit), "  --> Dauer[Min]: " + (minuten == 0 ? "<1" : minuten),
                    "========================================"});

        if (listeFilme.isEmpty()) {
            //Satz mit x, war wohl nix
            System.exit(1);
        } else {
            System.exit(0);
        }
    }

    private class BeobachterLadenFilme extends BeobFilmeLaden {

        @Override
        public void fertig(FilmListenerElement filmListenerElement) {
            undTschuess();
        }
    }
}
