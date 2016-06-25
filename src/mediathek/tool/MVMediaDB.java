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
package mediathek.tool;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.regex.Pattern;
import javax.swing.JOptionPane;
import mSearch.tool.ListenerMediathekView;
import mSearch.tool.MSConst;
import mSearch.tool.MSLog;
import mediathek.daten.Daten;
import mediathek.daten.DatenMediaDB;

public class MVMediaDB {

    private final ArrayList<DatenMediaDB> fileArray = new ArrayList<>(); //name-path-size
    public final String FILE_TRENNER = "<>";
    private boolean makeIndex = false;
    private String[] suffix = {""};
    private boolean ohneSuffix = true;

    public MVMediaDB() {
    }

    public synchronized int getSizeFileArray() {
        return fileArray.size();
    }

    public synchronized void getModelMediaDB(TModelMediaDB modelMediaDB) {
        modelMediaDB.setRowCount(0);
        for (DatenMediaDB s : fileArray) {
            modelMediaDB.addRow(s.getRow());
        }
    }

    public synchronized void searchFiles(TModelMediaDB modelFilm, String title) {

        modelFilm.setRowCount(0);
        if (!makeIndex && !title.isEmpty()) {
            Pattern p = Filter.makePattern(title);
            if (p != null) {
                // dann mit RegEx prüfen
                fileArray.stream().filter(s -> p.matcher(s.arr[DatenMediaDB.MEDIA_DB_NAME_NR]).matches()).forEach(s -> modelFilm.addRow(s.getRow()));
            } else {
                title = title.toLowerCase();
                for (DatenMediaDB s : fileArray) {
                    if (s.arr[DatenMediaDB.MEDIA_DB_NAME_NR].toLowerCase().contains(title)) {
                        modelFilm.addRow(s.getRow());
                    }
                }
            }
        }
    }

    public synchronized void makeIndex() {
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_MEDIA_DB_START, MVMediaDB.class.getSimpleName());
        suffix = Daten.mVConfig.get(MVConfig.SYSTEM_MEDIA_DB_SUFFIX).split(",");
        for (int i = 0; i < suffix.length; ++i) {
            suffix[i] = suffix[i].toLowerCase();
            if (!suffix[i].isEmpty() && !suffix[i].startsWith(".")) {
                suffix[i] = "." + suffix[i];
            }
        }
        ohneSuffix = Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_MEDIA_DB_SUFFIX_OHNE));

        makeIndex = true;
        fileArray.clear();
        new Thread(new Index()).start();
    }

    private class Index implements Runnable {

        @Override
        public synchronized void run() {
            try {
                String db = Daten.mVConfig.get(MVConfig.SYSTEM_MEDIA_DB_PATH_MEDIA);
                if (!db.isEmpty()) {
                    String error = "";
                    boolean more = false;
                    for (String s : db.split(FILE_TRENNER)) {
                        File f = new File(s);
                        if (!f.canRead()) {
                            if (!error.isEmpty()) {
                                error = error + "\n";
                                more = true;
                            }
                            error = error + f.getPath();
                        }
                    }
                    if (!error.isEmpty()) {
                        // Verzeichnisse können nicht durchsucht werden
                        MVMessageDialog.showMessageDialog(null, (more ? "Die Pfade der Mediensammlung können nicht gelesen werden:\n" : "Der Pfad der Mediensammlung kann nicht gelesen werden:\n")
                                + error, "Fehler beim Erstellen der Mediensammlung", JOptionPane.ERROR_MESSAGE);
                    }
                    for (String s : db.split(FILE_TRENNER)) {
                        File f = new File(s);
                        searchFile(f);
                    }
                }
            } catch (Exception ex) {
                MSLog.fehlerMeldung(120321254, ex);
            }
            makeIndex = false;
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_MEDIA_DB_STOP, MVMediaDB.class.getSimpleName());
        }

        private void searchFile(File dir) {
            if (dir == null) {
                return;
            }
            File[] files = dir.listFiles();
            if (files != null) {
                for (File file : files) {
                    if (file.isDirectory()) {
                        searchFile(file);
                    } else {
                        if (checkSuffix(suffix, file.getName())) {
                            fileArray.add(new DatenMediaDB(file.getName(), file.getParent().intern(), file.length()));
                        }
                    }
                }
            }
        }
    }

    private boolean checkSuffix(String[] str, String uurl) {
        // liefert TRUE wenn die Datei in die Mediensammlung kommt
        // prüfen ob url mit einem Argument in str endet
        // wenn str leer dann true
        if (str.length == 1 && str[0].isEmpty()) {
            return true;
        }

        boolean ret = true;
        final String url = uurl.toLowerCase();
        for (String s : str) {
            //Suffix prüfen
            if (ohneSuffix) {
                if (url.endsWith(s)) {
                    ret = false;
                    break;
                }
            } else {
                ret = false;
                if (url.endsWith(s)) {
                    ret = true;
                    break;
                }
            }
        }
        return ret;
    }

    public synchronized void writeFileArray(String datei) {
        OutputStreamWriter out = null;
        try {
            MVLog.systemMeldung("MediaDB schreiben (" + fileArray.size() + " Dateien) :");
            File file = new File(datei);
            File dir = new File(file.getParent());
            if (!dir.exists()) {
                if (!dir.mkdirs()) {
                    MSLog.fehlerMeldung(945120365, "Kann den Pfad nicht anlegen: " + dir.toString());
                }
            }
            MVLog.systemMeldung("   --> Start Schreiben nach: " + datei);
            out = new OutputStreamWriter(new FileOutputStream(datei), MSConst.KODIERUNG_UTF);

            for (DatenMediaDB s : fileArray) {
                out.write(s.arr[DatenMediaDB.MEDIA_DB_NAME_NR] + "\n");
            }
            MVLog.systemMeldung("   --> geschrieben!");
        } catch (Exception ex) {
            MSLog.fehlerMeldung(102035478, ex, "nach: " + datei);
        } finally {
            try {
                if (out != null) {
                    out.close();
                }
            } catch (Exception ignored) {
            }
        }
    }

}
