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
import java.util.ArrayList;
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

public class FilmlistenSuchen {
    //Tags FilmUpdateServer Filmliste

    public static final String FILM_UPDATE_SERVER_PRIO_1 = "1";
    public static final String FILM_UPDATE_SERVER = "film-update-server";
    public static final int FILM_UPDATE_SERVER_MAX_ELEM = 5;
    public static final String FILM_UPDATE_SERVER_NR = "film-update-server-nr";
    public static final int FILM_UPDATE_SERVER_NR_NR = 0;
    public static final String FILM_UPDATE_SERVER_URL = "film-update-server-url";
    public static final int FILM_UPDATE_SERVER_URL_NR = 1;
    public static final String FILM_UPDATE_SERVER_DATUM = "film-update-server-datum";
    public static final int FILM_UPDATE_SERVER_DATUM_NR = 2;
    public static final String FILM_UPDATE_SERVER_ZEIT = "film-update-server-zeit";
    public static final int FILM_UPDATE_SERVER_ZEIT_NR = 3;
    public static final String FILM_UPDATE_SERVER_PRIO = "film-update-server-prio";
    public static final int FILM_UPDATE_SERVER_PRIO_NR = 4;
    public static final String[] FILM_UPDATE_SERVER_COLUMN_NAMES = {FILM_UPDATE_SERVER_NR, FILM_UPDATE_SERVER_URL,
        FILM_UPDATE_SERVER_DATUM, FILM_UPDATE_SERVER_ZEIT, FILM_UPDATE_SERVER_PRIO};
    public static final String[] FILM_UPDATE_SERVER_COLUMN_NAMES_ANZEIGE = {"Nr", "Update-Url", "Datum", "Zeit", "Prio"};
    public ListeDownloadUrlsFilmlisten listeDownloadUrlsFilmlisten = new ListeDownloadUrlsFilmlisten();
    public ListeFilmlistenServer listeFilmlistenServer = new ListeFilmlistenServer();

    public String suchen(ArrayList<String> bereitsVersucht) {
        // passende URL zum Laden der Filmliste suchen
        String retUrl;
        ListeDownloadUrlsFilmlisten tmp = new ListeDownloadUrlsFilmlisten();
        try {
            if (DDaten.debug && !DDaten.system[Konstanten.SYSTEM_URL_FILMLISTEN_NR].equals("")) {
                // zum Debuggen
                getDownloadUrlsFilmlisten(DDaten.system[Konstanten.SYSTEM_URL_FILMLISTEN_NR], tmp, Daten.getUserAgent());
            } else {
                // Ausweichen auf andere Listenserver bei Bedarf
                getDownloadUrlsFilmlisten(Konstanten.ADRESSE_UPDATE_SERVER, tmp, Daten.getUserAgent());
                //getDownloadUrlsFilmlisten("asdf", tmp, Daten.getUserAgent());
                if (tmp.size() > 0) {
                    // dann die Liste Filmlistenserver aktualisieren
                    updateListeFilmlistenServer(tmp);
                    ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_FILMLISTEN_SERVER, this.getClass().getSimpleName());
                }
                if (tmp.size() == 0) {
                    // mit den Backuplisten versuchen
                    getDownloadUrlsFilmlisten__backuplisten(tmp, Daten.getUserAgent());
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(347895642, "FilmUpdateServer.suchen", ex);
        }
        if (tmp.size() == 0) {
            Log.systemMeldung(new String[]{"Es ist ein Fehler aufgetreten!",
                        "Es konnten keine Updateserver zum aktualisieren der Filme",
                        "gefunden werden."});
        } else {
            listeDownloadUrlsFilmlisten = tmp;
        }
        if (listeDownloadUrlsFilmlisten.size() < 5) {
            // dann gibts ein paar fest hinterlegt URLs
            listeDownloadUrlsFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_02.bz2", "1", "03:40:00", getTag("03:40:00")));
            listeDownloadUrlsFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_08.bz2", "1", "09:10:00", getTag("09:10:00")));
            listeDownloadUrlsFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_10.bz2", "1", "11:10:00", getTag("11:10:00")));
            listeDownloadUrlsFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek2/Mediathek_12.bz2", "1", "13:10:00", getTag("13:10:00")));
            listeDownloadUrlsFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek3/Mediathek_14.bz2", "1", "15:10:00", getTag("15:10:00")));
            listeDownloadUrlsFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek4/Mediathek_16.bz2", "1", "17:10:00", getTag("17:10:00")));
            listeDownloadUrlsFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_18.bz2", "1", "19:10:00", getTag("19:10:00")));
            listeDownloadUrlsFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_20.bz2", "1", "21:10:00", getTag("21:10:00")));
            listeDownloadUrlsFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_22.bz2", "1", "23:10:00", getTag("23:10:00")));
            listeDownloadUrlsFilmlisten.add(new DatenUrlFilmliste("http://176.28.14.91/mediathek1/Mediathek_00.bz2", "1", "01:10:00", getTag("01:10:00")));
        }
        listeDownloadUrlsFilmlisten.sort();
        retUrl = listeDownloadUrlsFilmlisten.getRand(bereitsVersucht, 0); //eine Zufällige Adresse wählen
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_URL_FILMLISTEN, this.getClass().getSimpleName());
        if (bereitsVersucht != null) {
            bereitsVersucht.add(retUrl);
        }
        return retUrl;
    }

    private void updateListeFilmlistenServer(ListeDownloadUrlsFilmlisten tmp) {
        Iterator<DatenUrlFilmliste> it = tmp.iterator();
        //listeFilmlistenServer.clear();
        while (it.hasNext()) {
            String serverUrl = it.next().arr[FILM_UPDATE_SERVER_URL_NR];
            String url = serverUrl.replace(GuiFunktionen.getDateiName(serverUrl), "");
            url = GuiFunktionen.addUrl(url, Konstanten.DATEINAME_LISTE_FILMLISTEN);
            listeFilmlistenServer.addCheck(new DatenFilmlistenServer(url));
        }
        // die Liste der Filmlistenserver aufräumen
        listeFilmlistenServer.alteLoeschen();
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

    private void getDownloadUrlsFilmlisten__backuplisten(ListeDownloadUrlsFilmlisten sListe, String userAgent) {
        getDownloadUrlsFilmlisten(GuiFunktionen.addUrl("http://176.28.14.91/mediathek1", Konstanten.DATEINAME_LISTE_FILMLISTEN), sListe, userAgent);
        getDownloadUrlsFilmlisten(GuiFunktionen.addUrl("http://176.28.14.91/mediathek2", Konstanten.DATEINAME_LISTE_FILMLISTEN), sListe, userAgent);
        getDownloadUrlsFilmlisten(GuiFunktionen.addUrl("http://176.28.14.91/mediathek3", Konstanten.DATEINAME_LISTE_FILMLISTEN), sListe, userAgent);
        getDownloadUrlsFilmlisten(GuiFunktionen.addUrl("http://176.28.14.91/mediathek4", Konstanten.DATEINAME_LISTE_FILMLISTEN), sListe, userAgent);
        Iterator<DatenFilmlistenServer> it = listeFilmlistenServer.iterator();
        while (it.hasNext()) {
            if (sListe.size() > 100) {
                // genug
                break;
            }
            DatenFilmlistenServer fs = it.next();
            getDownloadUrlsFilmlisten(fs.arr[DatenFilmlistenServer.FILM_LISTEN_SERVER_URL_NR], sListe, userAgent);
        }
    }

    public static void getDownloadUrlsFilmlisten(String dateiUrl, ListeDownloadUrlsFilmlisten sListe, String userAgent) {
        //String[] ret = new String[]{""/* version */, ""/* release */, ""/* updateUrl */};
        try {
            int event;
            XMLInputFactory inFactory = XMLInputFactory.newInstance();
            inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
            XMLStreamReader parser;
            InputStreamReader inReader;
            if (GuiFunktionen.istUrl(dateiUrl)) {
                // eine URL verarbeiten
                int timeout = 20000; //ms
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
                        //ret[0] = parser.getElementText();
                    } else if (parsername.equals("Program_Release_Info")) {
                        //ret[1] = parser.getElementText();
                    } else if (parsername.equals("Download_Programm")) {
                        //ret[2] = parser.getElementText();
                    } else if (parsername.equals("Server")) {
                        //wieder ein neuer Server, toll
                        getServer(parser, sListe);
                    }
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(821069874, FilmlistenSuchen.class.getName(), ex, "Die URL-Filmlisten konnte nicht geladen werden: " + dateiUrl);
        }
        //return ret;
    }

    private static void getServer(XMLStreamReader parser, ListeDownloadUrlsFilmlisten sListe) {
        String zeit = "";
        String datum = "";
        String serverUrl = "";
        String prio = "";
        int event;
        try {
            while (parser.hasNext()) {
                event = parser.next();
                if (event == XMLStreamConstants.START_ELEMENT) {
                    //parsername = parser.getLocalName();
                    if (parser.getLocalName().equals("Download_Filme_1")) {
                        serverUrl = parser.getElementText();
                    } else if (parser.getLocalName().equals("URL")) { // für die Zukunft
                        serverUrl = parser.getElementText();
                    } else if (parser.getLocalName().equals("Prio")) {
                        prio = parser.getElementText();
                    } else if (parser.getLocalName().equals("Datum")) {
                        datum = parser.getElementText();
                    } else if (parser.getLocalName().equals("Zeit")) {
                        zeit = parser.getElementText();
                    }
                }
                if (event == XMLStreamConstants.END_ELEMENT) {
                    //parsername = parser.getLocalName();
                    if (parser.getLocalName().equals("Server")) {
                        if (!serverUrl.equals("")) {
                            //public DatenFilmUpdate(String url, String prio, String zeit, String datum, String anzahl) {
                            if (prio.equals("")) {
                                prio = FilmlistenSuchen.FILM_UPDATE_SERVER_PRIO_1;
                            }
                            sListe.addWithCheck(new DatenUrlFilmliste(serverUrl, prio, zeit, datum));
                        }
                        break;
                    }
                }
            }
        } catch (XMLStreamException ex) {
        }

    }
}
