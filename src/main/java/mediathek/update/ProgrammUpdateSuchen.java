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
package mediathek.update;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Optional;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamReader;

import mSearch.tool.Log;
import mSearch.tool.Version;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;

public class ProgrammUpdateSuchen {
    private static final String UPDATE_SEARCH_TITLE = "Software-Aktualisierung";
    private static final String UPDATE_ERROR_MESSAGE = "<html>Es ist ein Fehler bei der Softwareaktualisierung aufgetreten.<br>" +
            "Die aktuelle Version konnte nicht ermittelt werden.</html>";
    /**
     * Connection timeout in milliseconds.
     */
    private static final int TIMEOUT = 10_000;
    private final ArrayList<String[]> listInfos = new ArrayList<>();
    private boolean neueVersion = false;

    public boolean checkVersion(boolean anzeigen, boolean showProgramInformation, boolean showAllInformation) {
        // prüft auf neue Version, aneigen: wenn true, dann AUCH wenn es keine neue Version gibt ein Fenster
        neueVersion = false;

        Optional<ServerProgramInformation> opt = retrieveProgramInformation();
        if (!opt.isPresent())
            SwingUtilities.invokeLater(() -> JOptionPane.showMessageDialog(null, UPDATE_ERROR_MESSAGE, UPDATE_SEARCH_TITLE, JOptionPane.ERROR_MESSAGE));
        else {
            // Update-Info anzeigen
            final ServerProgramInformation progInfo = opt.get();
            SwingUtilities.invokeLater(() -> {
                if (showProgramInformation)
                    showProgramInformation(showAllInformation);

                if (progInfo.getVersion().toNumber() == 0)
                    JOptionPane.showMessageDialog(null, UPDATE_ERROR_MESSAGE, UPDATE_SEARCH_TITLE, JOptionPane.ERROR_MESSAGE);
                else {
                    MVConfig.add(MVConfig.Configs.SYSTEM_BUILD_NR, Konstanten.MVVERSION.toString());
                    MVConfig.add(MVConfig.Configs.SYSTEM_UPDATE_DATUM, new SimpleDateFormat("yyyyMMdd").format(new Date()));

                    if (checkForNewerVersion(progInfo.getVersion())) {
                        neueVersion = true;
                        //TODO beautify this dialog. Looks really ugly.
                        new DialogHinweisUpdate(null, true, "Eine neue Version liegt vor",
                                "   ==================================================\n"
                                        + "   Neue Version:\n" + "   " + progInfo.getVersion() + "\n\n"
                                        + "   ==================================================\n"
                                        + "   Änderungen:\n" + "   " + progInfo.getReleaseNotes() + "\n\n"
                                        + "   ==================================================\n"
                                        + "   URL:\n"
                                        + "   " + progInfo.getUpdateUrl() + "\n\n").setVisible(true);

                    } else if (anzeigen) {
                        JOptionPane.showMessageDialog(null, "Sie benutzen die neueste Version von MediathekView.", UPDATE_SEARCH_TITLE, JOptionPane.INFORMATION_MESSAGE);
                    }
                }
            });
        }

        return neueVersion;
    }

    private void showProgramInformation(boolean showAll) {
        if (listInfos.isEmpty()) {
            //no info available
            if (showAll) {
                JOptionPane.showMessageDialog(null, "Es liegen keine Programminfos vor.", UPDATE_SEARCH_TITLE, JOptionPane.INFORMATION_MESSAGE);
            }
        } else {
            //display available info...
            try {
                StringBuilder text = new StringBuilder();
                int angezeigt = 0;
                if (MVConfig.get(MVConfig.Configs.SYSTEM_HINWEIS_NR_ANGEZEIGT).isEmpty()) {
                    MVConfig.add(MVConfig.Configs.SYSTEM_HINWEIS_NR_ANGEZEIGT, Integer.toString(-1));
                } else {
                    angezeigt = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_HINWEIS_NR_ANGEZEIGT));
                }
                int index = 0;
                for (String[] h : listInfos) {
                    index = Integer.parseInt(h[0]);
                    if (showAll || angezeigt < index) {
                        text.append("=======================================\n");
                        text.append(h[1]);
                        text.append('\n');
                        text.append('\n');
                    }
                }
                if (text.length() > 0) {
                    new DialogHinweisUpdate(null, true, "Infos", text.toString()).setVisible(true);
                    MVConfig.add(MVConfig.Configs.SYSTEM_HINWEIS_NR_ANGEZEIGT, Integer.toString(index));
                }
            } catch (Exception ex) {
                Log.errorLog(693298731, ex);
            }
        }
    }

    /**
     * Check if a newer version exists.
     *
     * @param info the remote version number.
     * @return true if there is a newer version
     */
    private boolean checkForNewerVersion(Version info) {
        return (Konstanten.MVVERSION.compare(info) == 1);
    }

    private InputStream connectToServer() throws IOException {
        URLConnection conn = new URL(Konstanten.ADRESSE_PROGRAMM_VERSION).openConnection();
        conn.setRequestProperty("User-Agent", Daten.getUserAgent());
        conn.setReadTimeout(TIMEOUT);
        conn.setConnectTimeout(TIMEOUT);

        return conn.getInputStream();
    }

    /**
     * Load and parse the update information.
     *
     * @return parsed update info for further use when successful
     */
    private Optional<ServerProgramInformation> retrieveProgramInformation() {
        int event;
        XMLStreamReader parser = null;
        ServerProgramInformation progInfo;

        XMLInputFactory inFactory = XMLInputFactory.newInstance();
        inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);

        try (InputStreamReader inReader = new InputStreamReader(connectToServer(), StandardCharsets.UTF_8)) {
            parser = inFactory.createXMLStreamReader(inReader);
            progInfo = new ServerProgramInformation();

            while (parser.hasNext()) {
                event = parser.next();
                if (event == XMLStreamConstants.START_ELEMENT) {
                    switch (parser.getLocalName()) {
                        case ServerProgramInformation.ParserTags.VERSION:
                            progInfo.setVersion(parser.getElementText());
                            break;
                        case ServerProgramInformation.ParserTags.RELEASE_NOTES:
                            progInfo.setReleaseNotes(parser.getElementText());
                            break;
                        case ServerProgramInformation.ParserTags.UPDATE_URL:
                            progInfo.setUpdateUrl(parser.getElementText());
                            break;
                        case ServerProgramInformation.ParserTags.INFO:
                            int count = parser.getAttributeCount();
                            String nummer = "";
                            for (int i = 0; i < count; ++i) {
                                if (parser.getAttributeName(i).toString().equals(ServerProgramInformation.ParserTags.INFO_NO)) {
                                    nummer = parser.getAttributeValue(i);
                                }
                            }
                            String info = parser.getElementText();
                            if (!nummer.isEmpty() && !info.isEmpty()) {
                                listInfos.add(new String[]{nummer, info});
                            }
                            break;
                        default:
                            break;
                    }
                }
            }
            return Optional.of(progInfo);
        } catch (Exception ex) {
            return Optional.empty();
        } finally {
            try {
                if (parser != null)
                    parser.close();
            } catch (Exception ignored) {
            }
        }
    }
}
