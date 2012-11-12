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
package mediathek.controller.filmeLaden.importieren;

import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.tool.DatumZeit;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class FilmlistenServer {
    //Tags FilmUpdateServer Filmliste

    public static final String FILM_UPDATE_SERVER_PRIO_1 = "1";
    public static final String FILM_UPDATE_SERVER_PRIO_2 = "2";
    public static final String FILM_UPDATE_SERVER = "film-update-server";
    public static final int FILM_UPDATE_SERVER_MAX_ELEM = 6;
    public static final String FILM_UPDATE_SERVER_NR = "film-update-server-nr";
    public static final int FILM_UPDATE_SERVER_NR_NR = 0;
    public static final String FILM_UPDATE_SERVER_URL = "film-update-server-url";
    public static final int FILM_UPDATE_SERVER_URL_NR = 1;
    public static final String FILM_UPDATE_SERVER_DATUM = "film-update-server-datum";
    public static final int FILM_UPDATE_SERVER_DATUM_NR = 2;
    public static final String FILM_UPDATE_SERVER_ZEIT = "film-update-server-zeit";
    public static final int FILM_UPDATE_SERVER_ZEIT_NR = 3;
    public static final String FILM_UPDATE_SERVER_ANZAHL = "film-update-server-anzahl";
    public static final int FILM_UPDATE_SERVER_ANZAHL_NR = 4;
    public static final String FILM_UPDATE_SERVER_PRIO = "film-update-server-prio";
    public static final int FILM_UPDATE_SERVER_PRIO_NR = 5;
    public static final String[] FILM_UPDATE_SERVER_COLUMN_NAMES = {FILM_UPDATE_SERVER_NR, FILM_UPDATE_SERVER_URL, FILM_UPDATE_SERVER_DATUM, FILM_UPDATE_SERVER_ZEIT, FILM_UPDATE_SERVER_ANZAHL, FILM_UPDATE_SERVER_PRIO};
    public static final String[] FILM_UPDATE_SERVER_COLUMN_NAMES_ANZEIGE = {"Nr", "Update-Url", "Datum", "Zeit", "Anzahl", FILM_UPDATE_SERVER_PRIO};
    public ListeUrlFilmlisten listeUrlFilmlisten = new ListeUrlFilmlisten();
    public ListeFilmlistenServer listeFilmlistenServer = new ListeFilmlistenServer();

    public String suchen() {
        String retUrl;
        ListeUrlFilmlisten tmp = new ListeUrlFilmlisten();
        try {
            ////////////////////////
            // Ausweichen auf andere Listenserver bei Bedarf
            if (DDaten.debug && !DDaten.system[Konstanten.SYSTEM_URL_FILMLISTEN_NR].equals("")) {
                getFilmlisten(DDaten.system[Konstanten.SYSTEM_URL_FILMLISTEN_NR], tmp, Daten.getUserAgent());
            } else {
                getFilmlisten(Konstanten.ADRESSE_UPDATE_SERVER, tmp, Daten.getUserAgent());
                ///getFilmlisten("asdf", tmp, Daten.getUserAgent());
                if (tmp.size() == 0) {
                    // neue URL Filmlisten versuchen
                    ////////////////////
                } else {
                    // dann die Liste Filmlistenserver aktualisieren
                    Iterator<DatenUrlFilmliste> it = tmp.iterator();
                    listeFilmlistenServer.clear();
                    while (it.hasNext()) {
                        String serverUrl = it.next().arr[FILM_UPDATE_SERVER_URL_NR];
                        String url = serverUrl.replace(GuiFunktionen.getDateiName(serverUrl), "");
                        url = GuiFunktionen.addUrl(url, Konstanten.DATEINAME_LISTE_FILMLISTEN);
                        listeFilmlistenServer.addCheck(new DatenFilmlistenServer(url));
                    }
                    ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_FILMLISTEN_SERVER, this.getClass().getSimpleName());
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(347895642, "FilmUpdateServer.suchen", ex);
        }
        if (tmp.isEmpty()) {
            Log.systemMeldung(new String[]{"Es ist ein Fehler aufgetreten!",
                        "Es konnten keine Updateserver zum aktualisieren der Filme",
                        "gefunden werden."});
        } else {
            listeUrlFilmlisten = tmp;
        }
        if (listeUrlFilmlisten.size() == 0) {
            listeUrlFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_02.bz2", "1", "03:40:00", getTag("03:40:00")));
            listeUrlFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_06.bz2", "1", "07:10:00", getTag("07:10:00")));
            listeUrlFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_08.bz2", "1", "09:10:00", getTag("09:10:00")));
            listeUrlFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_10.bz2", "1", "11:10:00", getTag("11:10:00")));
            listeUrlFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek2/Mediathek_12.bz2", "1", "13:10:00", getTag("13:10:00")));
            listeUrlFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek3/Mediathek_14.bz2", "1", "15:10:00", getTag("15:10:00")));
            listeUrlFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek4/Mediathek_16.bz2", "1", "17:10:00", getTag("17:10:00")));
            listeUrlFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_18.bz2", "1", "19:10:00", getTag("19:10:00")));
            listeUrlFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_20.bz2", "1", "21:10:00", getTag("21:10:00")));
            listeUrlFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_22.bz2", "1", "23:10:00", getTag("23:10:00")));
            listeUrlFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_00.bz2", "1", "01:10:00", getTag("01:10:00")));
        }
        listeUrlFilmlisten.sort();
        retUrl = listeUrlFilmlisten.getRand(0); //eine Zufällige Adresse wählen
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_URL_FILMLISTEN, this.getClass().getSimpleName());
        return retUrl;
    }

    private String getTag(String zeit) {
        Date tmp;
        SimpleDateFormat sdf_zeit = new SimpleDateFormat("dd.MM.yyyy__HH:mm:ss");
        try {
            tmp = sdf_zeit.parse(DatumZeit.getHeute_dd_MM_yyyy() + "__" + zeit);
            if (tmp.compareTo(new Date()) > 0) {
                return DatumZeit.getGestern_dd_MM_yyyy();
            } else {
                return DatumZeit.getHeute_dd_MM_yyyy();
            }
        } catch (Exception ex) {
        }
        return DatumZeit.getHeute_dd_MM_yyyy();
    }

    public static String[] getFilmlisten(String dateiUrl, ListeUrlFilmlisten sListe, String userAgent) {
        String[] ret = new String[]{""/* version */, ""/* release */, ""/* updateUrl */};
        sListe.clear();
        try {
            int event;
            XMLInputFactory inFactory = XMLInputFactory.newInstance();
            inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
            XMLStreamReader parser;
            InputStreamReader inReader;
            if (GuiFunktionen.istUrl(dateiUrl)) {
                // eine URL verarbeiten
                int timeout = 10000; //ms
                URLConnection conn;
                conn = new URL(dateiUrl).openConnection();
                conn.setRequestProperty("User-Agent", userAgent);
                conn.setReadTimeout(timeout);
                conn.setConnectTimeout(timeout);
                inReader = new InputStreamReader(conn.getInputStream(), Konstanten.KODIERUNG_UTF);
            } else {
                // eine Datei verarbeiten
                inReader = new InputStreamReader(new FileInputStream(dateiUrl), Konstanten.KODIERUNG_UTF);
            }
            parser = inFactory.createXMLStreamReader(inReader);
            while (parser.hasNext()) {
                event = parser.next();
                if (event == XMLStreamConstants.START_ELEMENT) {
                    String parsername = parser.getLocalName();
                    if (parsername.equals("Program_Version")) {
                        ret[0] = parser.getElementText();
                    } else if (parsername.equals("Program_Release_Info")) {
                        ret[1] = parser.getElementText();
                    } else if (parsername.equals("Download_Programm")) {
                        ret[2] = parser.getElementText();
                    } else if (parsername.equals("Server")) {
                        //wieder ein neuer Server, toll
                        getServer(parser, sListe);
                    }
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(821069874, FilmlistenServer.class.getName(), ex, "Die URL-Filmlisten konnte nicht geladen werden: " + dateiUrl);
        }
        return ret;
    }

    private static void getServer(XMLStreamReader parser, ListeUrlFilmlisten sListe) {
        String zeit = "";
        String datum = "";
        String serverUrl = "";
        //String parsername = "";
        String prio;
        int event;
        try {
            while (parser.hasNext()) {
                prio = FilmlistenServer.FILM_UPDATE_SERVER_PRIO_1;
                event = parser.next();
                if (event == XMLStreamConstants.END_ELEMENT) {
                    //parsername = parser.getLocalName();
                    if (parser.getLocalName().equals("Server")) {
                        if (!serverUrl.equals("")) {
                            //public DatenFilmUpdate(String url, String prio, String zeit, String datum, String anzahl) {
                            sListe.addWithCheck(new DatenUrlFilmliste(serverUrl, prio, zeit, datum));
                        }
                        break;
                    }
                }
                if (event == XMLStreamConstants.START_ELEMENT) {
                    //parsername = parser.getLocalName();
                    if (parser.getLocalName().equals("Download_Filme_1")) {
                        serverUrl = parser.getElementText();
                        prio = FilmlistenServer.FILM_UPDATE_SERVER_PRIO_1;
                    } else if (parser.getLocalName().equals("Download_Filme_2")) {
                        serverUrl = parser.getElementText();
                        prio = FilmlistenServer.FILM_UPDATE_SERVER_PRIO_2;
                    } else if (parser.getLocalName().equals("Datum")) {
                        datum = parser.getElementText();
                    } else if (parser.getLocalName().equals("Zeit")) {
                        zeit = parser.getElementText();
                    }
                }
            }
        } catch (XMLStreamException ex) {
        }

    }
}
