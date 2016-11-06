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
package mediathek.controller;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import javax.swing.SwingUtilities;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import mSearch.Const;
import mSearch.tool.Functions;
import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.gui.dialog.DialogHinweisUpdate;

public class ProgrammUpdateSuchen {

    private final static String PROGRAM_VERSION = "Program_Version";
    private final static String PROGRAM_RELEASE_INFO = "Program_Release_Info";
    private final static String DOWNLOAD_PROGRAM = "Download_Programm";
    private final static String INFO = "Info";
    private final static String INFO_NO = "number";
    private final LinkedList<String[]> listInfos = new LinkedList<>(); // String[] info = {Nummer, Info};
    private String version;
    private String release;
    private String downloadUrlProgramm;
    private String[] progInfoArr;
    private boolean neueVersion = false;

    public boolean checkVersion(boolean anzeigen, boolean hinweisAnzeigen, boolean alleHinweiseAnzeigen) {
        // prüft auf neue Version, aneigen: wenn true, dann AUCH wenn es keine neue Version gibt ein Fenster
        neueVersion = false;

        try {
            progInfoArr = progInfosSuchen();
            // Update-Info anzeigen
            version = progInfoArr[0];
            release = progInfoArr[1];
            downloadUrlProgramm = progInfoArr[2];

            SwingUtilities.invokeLater(() -> {
                // Hinweise anzeigen
                if (hinweisAnzeigen) {
                    hinweiseAnzeigen(alleHinweiseAnzeigen);
                }

                if (version.equals("")) {
                    new DialogHinweisUpdate(null, true, "Fehler bei der Versionsprüfung!", "Es ist ein Fehler aufgetreten!" + "\n\n" + "").setVisible(true);
                } else {
                    MVConfig.add(MVConfig.Configs.SYSTEM_BUILD_NR, Functions.getBuildNr());
                    MVConfig.add(MVConfig.Configs.SYSTEM_UPDATE_DATUM, new SimpleDateFormat("yyyyMMdd").format(new Date()));

                    if (checkObNeueVersion(version, Const.VERSION)) {
                        neueVersion = true;
                        // DialogHinweisUpdate(java.awt.Frame parent, boolean modal, String ttext, String dialogTitel, Daten ddaten) {
                        new DialogHinweisUpdate(null, true, "Eine neue Version liegt vor",
                                "   ==================================================\n"
                                + "   Neue Version:\n" + "   " + version + "\n\n"
                                + "   ==================================================\n"
                                + "   Änderungen:\n" + "   " + release + "\n\n"
                                + "   ==================================================\n"
                                + "   URL:\n"
                                + "   " + downloadUrlProgramm + "\n\n").setVisible(true);

                    } else if (anzeigen) {
                        new DialogHinweisUpdate(null, true, "Update suchen", "Alles aktuell!").setVisible(true);
                    }
                }
            });
        } catch (Exception ex) {
            Log.errorLog(159002583, ex);
        }
        return neueVersion;
    }

    private void hinweiseAnzeigen(boolean alleAnzeigen) {
        if (listInfos.size() > 0) {
            try {
                StringBuilder text = new StringBuilder();
                int angezeigt = 0;
                if (MVConfig.get(MVConfig.Configs.SYSTEM_HINWEIS_NR_ANGEZEIGT).equals("")) {
                    MVConfig.add(MVConfig.Configs.SYSTEM_HINWEIS_NR_ANGEZEIGT, Integer.toString(-1));
                } else {
                    angezeigt = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_HINWEIS_NR_ANGEZEIGT));
                }
                int idx = 0;
                for (String[] h : listInfos) {
                    idx = Integer.parseInt(h[0]);
                    if (alleAnzeigen || angezeigt < idx) {
                        text.append("=======================================\n");
                        text.append(h[1]);
                        text.append("\n");
                        text.append("\n");
                    }
                }
                if (text.length() > 0) {
                    new DialogHinweisUpdate(null, true, "Infos", text.toString()).setVisible(true);
                    MVConfig.add(MVConfig.Configs.SYSTEM_HINWEIS_NR_ANGEZEIGT, Integer.toString(idx));
                }
            } catch (Exception ex) {
                Log.errorLog(693298731, ex);
            }
        } else if (alleAnzeigen) {
            // dann wenigstens einen Hinweis, dass es keine gibt
            new DialogHinweisUpdate(null, true, "Infos", "keine vorhanden").setVisible(true);
        }
    }

    /**
     * Check if a newer version exists.
     *
     * @param infoVersion
     * @param currentVersion
     * @return true if there is a newer version
     */
    private boolean checkObNeueVersion(String infoVersion, String currentVersion) {
        //FIXME Get rid of the strings as we are converting to int anyway!!!!
        try {
            // erste stelle
            int info = Integer.parseInt(infoVersion);
            int ich = Integer.parseInt(currentVersion);
            if (info > ich) {
                return true;
            }
        } catch (Exception ex) {
            Log.errorLog(683021193, ex);
        }
        return false;
    }

    private String[] progInfosSuchen() throws IOException, XMLStreamException {
        String[] retArr = new String[]{""/* version */, ""/* release */, ""/* updateUrl */};
        //String parsername = "";
        int event;
        XMLInputFactory inFactory = XMLInputFactory.newInstance();
        inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
        XMLStreamReader parser;
        InputStreamReader inReader;
        int timeout = 10000;
        URLConnection conn;
        conn = new URL(Konstanten.ADRESSE_PROGRAMM_VERSION).openConnection();
        conn.setRequestProperty("User-Agent", Daten.getUserAgent());
        conn.setReadTimeout(timeout);
        conn.setConnectTimeout(timeout);
        inReader = new InputStreamReader(conn.getInputStream(), Const.KODIERUNG_UTF);
        parser = inFactory.createXMLStreamReader(inReader);
        while (parser.hasNext()) {
            event = parser.next();
            if (event == XMLStreamConstants.START_ELEMENT) {
                //parsername = parser.getLocalName();
                switch (parser.getLocalName()) {
                    case PROGRAM_VERSION:
                        retArr[0] = parser.getElementText();
                        break;
                    case PROGRAM_RELEASE_INFO:
                        retArr[1] = parser.getElementText();
                        break;
                    case DOWNLOAD_PROGRAM:
                        retArr[2] = parser.getElementText();
                        break;
                    case INFO:
                        int count = parser.getAttributeCount();
                        String nummer = "";
                        for (int i = 0; i < count; ++i) {
                            if (parser.getAttributeName(i).toString().equals(INFO_NO)) {
                                nummer = parser.getAttributeValue(i);
                            }
                        }
                        String info = parser.getElementText();
                        if (!nummer.equals("") && !info.equals("")) {
                            listInfos.add(new String[]{nummer, info});
                        }
                        break;
                    default:
                        break;
                }
            }
        }
        return retArr;
    }
}
