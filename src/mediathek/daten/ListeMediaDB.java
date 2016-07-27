/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
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
package mediathek.daten;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedList;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import mSearch.tool.Functions;
import mSearch.tool.Log;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.tool.MVMessageDialog;
import mediathek.tool.TModelMediaDB;

public class ListeMediaDB extends LinkedList<DatenMediaDB> {

    public final static String TRENNER = "  |###|  ";

    public synchronized void getModelMediaDB(TModelMediaDB modelMediaDB) {
        modelMediaDB.setRowCount(0);
        this.stream().forEach((mdb) -> {
            modelMediaDB.addRow(mdb.getRow());
        });
    }

//    public void listeBauen() {
//        Path urlPath = getFilePath();
//        //use Automatic Resource Management
//        try (LineNumberReader in = new LineNumberReader(new InputStreamReader(Files.newInputStream(urlPath)))) {
//            String zeile;
//            while ((zeile = in.readLine()) != null) {
//                DatenMediaDB mdb = getUrlAusZeile(zeile);
//                add(mdb);
//            }
//        } catch (Exception ex) {
//            Log.errorLog(926362547, ex);
//        }
//    }
    public void exportListe(String datei) {
        Path logFilePath = null;
        boolean export = false;
        SysMsg.sysMsg("MediaDB schreiben (" + Daten.listeMediaDB.size() + " Dateien) :");
        if (!datei.isEmpty()) {
            export = true;
            try {
                File file = new File(datei);
                File dir = new File(file.getParent());
                if (!dir.exists()) {
                    if (!dir.mkdirs()) {
                        Log.errorLog(945120365, "Kann den Pfad nicht anlegen: " + dir.toString());
                    }
                }
                SysMsg.sysMsg("   --> Start Schreiben nach: " + datei);
                logFilePath = file.toPath();
            } catch (Exception ex) {
                Log.errorLog(102035478, ex, "nach: " + datei);
            }
        } else {
            SysMsg.sysMsg("   --> Start Schreiben nach: " + getFilePath().toString());
            logFilePath = getFilePath();
        }

        try (BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(logFilePath)))) {
            bw.newLine();
            bw.newLine();
            for (DatenMediaDB entry : this) {
                bw.write(getLine(entry, export));
                bw.newLine();
            }
            bw.newLine();
            //
            bw.flush();
            bw.close();
        } catch (Exception ex) {
            SwingUtilities.invokeLater(() -> {
                MVMessageDialog.showMessageDialog(null, "Datei konnte nicht geschrieben werden!",
                        "Fehler beim Schreiben", JOptionPane.ERROR_MESSAGE);
            });
        }
        SysMsg.sysMsg("   --> geschrieben!");
    }

    private String getLine(DatenMediaDB med, boolean export) {
        if (export) {
            return med.arr[DatenMediaDB.MEDIA_DB_NAME];
        }
        String ret = "";
        ret += Functions.minTextLaenge(60, med.arr[DatenMediaDB.MEDIA_DB_NAME]) + TRENNER;
        ret += Functions.minTextLaenge(60, med.arr[DatenMediaDB.MEDIA_DB_PATH]) + TRENNER;
        ret += Functions.minTextLaenge(6, med.arr[DatenMediaDB.MEDIA_DB_SIZE]);
        return ret;
    }

    private Path getFilePath() {
        Path urlPath = null;
        try {
            urlPath = Paths.get(Daten.getSettingsDirectory_String()).resolve(Konstanten.FILE_MEDIA_DB);
            if (Files.notExists(urlPath)) {
                urlPath = Files.createFile(urlPath);
            }
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return urlPath;
    }

//    private DatenMediaDB getUrlAusZeile(String zeile) {
//        String name = "", pfad = "", s = "";
//        long size = 0;
//        try {
//            if (zeile.contains(TRENNER)) {
//                //neues Logfile-Format
//                int a1 = zeile.lastIndexOf(TRENNER);
//                a1 += TRENNER.length();
//                pfad = zeile.substring(a1).trim();
//                s = zeile.substring(zeile.lastIndexOf(TRENNER) + TRENNER.length(), zeile.lastIndexOf(TRENNER)).trim();
//                name = zeile.substring(0, zeile.indexOf(TRENNER)).trim();
//            }
//        } catch (Exception ex) {
//            Log.errorLog(912035647, ex);
//        }
//        if (!s.isEmpty()) {
//            try {
//                size = Integer.parseInt(s);
//            } catch (Exception ignore) {
//                size = 0;
//            }
//        }
//        return new DatenMediaDB(name, pfad, size);
//    }
}
