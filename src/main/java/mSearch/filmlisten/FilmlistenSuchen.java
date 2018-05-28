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
package mSearch.filmlisten;

import mSearch.Const;
import mSearch.tool.ApplicationConfiguration;
import mSearch.tool.Functions;
import mSearch.tool.Log;
import mSearch.tool.MVHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.commons.configuration2.Configuration;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Random;

public class FilmlistenSuchen {

    // damit werden die DownloadURLs zum Laden einer Filmliste gesucht
    // Liste mit den URLs zum Download der Filmliste
    public ListeFilmlistenUrls listeFilmlistenUrls_akt = new ListeFilmlistenUrls();
    public ListeFilmlistenUrls listeFilmlistenUrls_diff = new ListeFilmlistenUrls();
    private static boolean firstSearchAkt = true;
    private static boolean firstSearchDiff = true;
    private static final int UPDATE_LISTE_MAX = 10; // die Downloadliste für die Filmlisten nur jeden 10. Programmstart aktualisieren


    /**
     * Force update of update server urls based on config.
     *
     * @param liste     akt of diff url list
     * @param configStr config setting to use
     */
    private void forceUpdateServerReload(ListeFilmlistenUrls liste, String configStr) {
        final Configuration config = ApplicationConfiguration.getConfiguration();
        final boolean forceReload = config.getBoolean(configStr, true);
        if (forceReload) {
            //just delete all entries. they will be updated...
            liste.clear();
            config.setProperty(configStr, false);
        }
    }

    public String suchenAkt(ArrayList<String> bereitsVersucht) {
        // passende URL zum Laden der Filmliste suchen
        String retUrl;

        forceUpdateServerReload(listeFilmlistenUrls_akt, ApplicationConfiguration.APPLICATION_FORCE_UPDATE_SERVER_RELOAD_AKT);

        if (listeFilmlistenUrls_akt.isEmpty()) {
            // bei leerer Liste immer aktualisieren
            updateURLsFilmlisten(true);
        } else if (firstSearchAkt) {
            // nach dem Programmstart wird die Liste einmal aktualisiert aber
            // da sich die Listen nicht ändern, nur jeden xx Start
            int nr = new Random().nextInt(UPDATE_LISTE_MAX);
            if (nr == 0) {
                updateURLsFilmlisten(true);
            }
        }
        firstSearchAkt = false;
        retUrl = (listeFilmlistenUrls_akt.getRand(bereitsVersucht)); //eine Zufällige Adresse wählen
        if (bereitsVersucht != null) {
            bereitsVersucht.add(retUrl);
        }
        return retUrl;
    }

    public String suchenDiff(ArrayList<String> bereitsVersucht) {
        // passende URL zum Laden der Filmliste suchen
        String retUrl;

        forceUpdateServerReload(listeFilmlistenUrls_diff, ApplicationConfiguration.APPLICATION_FORCE_UPDATE_SERVER_RELOAD_DIFF);

        if (listeFilmlistenUrls_diff.isEmpty()) {
            // bei leerer Liste immer aktualisieren
            updateURLsFilmlisten(false);
        } else if (firstSearchDiff) {
            // nach dem Programmstart wird die Liste einmal aktualisiert aber
            // da sich die Listen nicht ändern, nur jeden xx Start
            int nr = new Random().nextInt(UPDATE_LISTE_MAX);
            if (nr == 0) {
                updateURLsFilmlisten(false);
            }
        }
        firstSearchDiff = false;
        retUrl = (listeFilmlistenUrls_diff.getRand(bereitsVersucht)); //eine Zufällige Adresse wählen
        if (bereitsVersucht != null) {
            bereitsVersucht.add(retUrl);
        }
        return retUrl;
    }

    /**
     * Add our default full list servers.
     */
    private void insertDefaultActiveServers() {
        listeFilmlistenUrls_akt.add(new DatenFilmlisteUrl("http://verteiler1.mediathekview.de/Filmliste-akt.xz", DatenFilmlisteUrl.SERVER_ART_AKT));
        listeFilmlistenUrls_akt.add(new DatenFilmlisteUrl("http://verteiler2.mediathekview.de/Filmliste-akt.xz", DatenFilmlisteUrl.SERVER_ART_AKT));
        listeFilmlistenUrls_akt.add(new DatenFilmlisteUrl("http://verteiler3.mediathekview.de/Filmliste-akt.xz", DatenFilmlisteUrl.SERVER_ART_AKT));
        listeFilmlistenUrls_akt.add(new DatenFilmlisteUrl("http://verteiler4.mediathekview.de/Filmliste-akt.xz", DatenFilmlisteUrl.SERVER_ART_AKT));
    }

    /**
     * Add our default diff list servers.
     */
    private void insertDefaultDifferentialListServers() {
        listeFilmlistenUrls_diff.add(new DatenFilmlisteUrl("http://verteiler1.mediathekview.de/Filmliste-diff.xz", DatenFilmlisteUrl.SERVER_ART_DIFF));
        listeFilmlistenUrls_diff.add(new DatenFilmlisteUrl("http://verteiler2.mediathekview.de/Filmliste-diff.xz", DatenFilmlisteUrl.SERVER_ART_DIFF));
        listeFilmlistenUrls_diff.add(new DatenFilmlisteUrl("http://verteiler3.mediathekview.de/Filmliste-diff.xz", DatenFilmlisteUrl.SERVER_ART_DIFF));
        listeFilmlistenUrls_diff.add(new DatenFilmlisteUrl("http://verteiler4.mediathekview.de/Filmliste-diff.xz", DatenFilmlisteUrl.SERVER_ART_DIFF));
    }

    /**
     * Update the download server URLs.
     *
     * @param updateFullList if true, update full list server, otherwise diff servers.
     **/
    public void updateURLsFilmlisten(final boolean updateFullList) {
        ListeFilmlistenUrls tmp = new ListeFilmlistenUrls();
        if (updateFullList) {
            getFilmlistServerUrls(Const.ADRESSE_FILMLISTEN_SERVER_AKT, tmp, DatenFilmlisteUrl.SERVER_ART_AKT);
            if (!tmp.isEmpty()) {
                listeFilmlistenUrls_akt = tmp;
            } else if (listeFilmlistenUrls_akt.isEmpty()) {
                insertDefaultActiveServers();
            }
            listeFilmlistenUrls_akt.sort();
        } else {
            getFilmlistServerUrls(Const.ADRESSE_FILMLISTEN_SERVER_DIFF, tmp, DatenFilmlisteUrl.SERVER_ART_DIFF);
            if (!tmp.isEmpty()) {
                listeFilmlistenUrls_diff = tmp;
            } else if (listeFilmlistenUrls_diff.isEmpty()) {
                insertDefaultDifferentialListServers();
            }
            listeFilmlistenUrls_diff.sort();
        }
        if (tmp.isEmpty()) {
            Log.errorLog(491203216, new String[]{"Es ist ein Fehler aufgetreten!",
                    "Es konnten keine Updateserver zum aktualisieren der Filme",
                    "gefunden werden."});
        }
    }

    /**
     * Read server urls from file.
     */
    private void readFile(String dateiUrl, ListeFilmlistenUrls listeFilmlistenUrls, String art) {
        final Path filePath = Paths.get(dateiUrl);
        if (!Files.exists(filePath))
            return;

        XMLStreamReader parser = null;
        try (InputStream is = Files.newInputStream(filePath);
             InputStreamReader inReader = new InputStreamReader(is, StandardCharsets.UTF_8)) {
            parser = inFactory.createXMLStreamReader(inReader);
            parseData(parser, listeFilmlistenUrls, art);
        } catch (Exception ex) {
            ex.printStackTrace();
        } finally {
            if (parser != null) {
                try {
                    parser.close();
                } catch (XMLStreamException ignored) {
                }
            }
        }
    }

    private void parseData(XMLStreamReader parser, ListeFilmlistenUrls listeFilmlistenUrls, String art) throws XMLStreamException {
        while (parser.hasNext()) {
            final int event = parser.next();
            if (event == XMLStreamConstants.START_ELEMENT) {
                String parsername = parser.getLocalName();
                if (parsername.equals("Server")) {
                    parseServerEntry(parser, listeFilmlistenUrls, art);
                }
            }
        }
    }

    private final XMLInputFactory inFactory;

    public FilmlistenSuchen() {
        inFactory = XMLInputFactory.newInstance();
        inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
    }

    /**
     * Read server urls from web location.
     */
    private void readWebLocation(URL dateiUrl, ListeFilmlistenUrls listeFilmlistenUrls, String art) {

        final Request request = new Request.Builder().url(dateiUrl).get().build();
        try (Response response = MVHttpClient.getInstance().getReducedTimeOutClient().newCall(request).execute();
             ResponseBody body = response.body()) {
            if (response.isSuccessful() && body != null) {
                //work here...
                XMLStreamReader parser = null;
                try (InputStream is = body.byteStream();
                     InputStreamReader inReader = new InputStreamReader(is, StandardCharsets.UTF_8)) {
                    parser = inFactory.createXMLStreamReader(inReader);
                    parseData(parser, listeFilmlistenUrls, art);
                } finally {
                    if (parser != null)
                        parser.close();
                }
            } else
                throw new Exception();
        } catch (Exception ex) {
            Log.errorLog(821069874, ex, "Die URL-Filmlisten konnte nicht geladen werden: " + dateiUrl);
        }
    }

    private void getFilmlistServerUrls(String dateiUrl, ListeFilmlistenUrls listeFilmlistenUrls, String art) {
        //String[] ret = new String[]{""/* version */, ""/* release */, ""/* updateUrl */};
        try {
            if (Functions.istUrl(dateiUrl))
                readWebLocation(new URL(dateiUrl), listeFilmlistenUrls, art);
            else
                readFile(dateiUrl, listeFilmlistenUrls, art);
        } catch (MalformedURLException ex) {
            Log.sysLog("Fehlerhafte Update-Server-URL gelesen: " + dateiUrl);
        }
    }

    /**
     * Parse the server XML file.
     */
    private void parseServerEntry(XMLStreamReader parser, ListeFilmlistenUrls listeFilmlistenUrls, String art) {
        String serverUrl = "";
        String prio = "";
        try {
            while (parser.hasNext()) {
                final int event = parser.next();
                if (event == XMLStreamConstants.START_ELEMENT) {
                    switch (parser.getLocalName()) {
                        case "URL":
                            serverUrl = parser.getElementText();
                            break;
                        case "Prio":
                            prio = parser.getElementText();
                            break;
                    }
                }
                if (event == XMLStreamConstants.END_ELEMENT) {
                    if (parser.getLocalName().equals("Server")) {
                        if (!serverUrl.isEmpty()) {
                            if (prio.isEmpty()) {
                                prio = DatenFilmlisteUrl.FILM_UPDATE_SERVER_PRIO_1;
                            }
                            listeFilmlistenUrls.addWithCheck(new DatenFilmlisteUrl(serverUrl, prio, art));
                        }
                        break;
                    }
                }
            }
        } catch (XMLStreamException ignored) {
        }

    }

}
