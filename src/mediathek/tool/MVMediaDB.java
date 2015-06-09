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
import mediathek.controller.Log;
import mediathek.daten.Daten;
import static mediathek.tool.Filter.makePattern;
import msearch.tool.MSConst;
import msearch.tool.MSLog;

public class MVMediaDB {

    private final ArrayList<String[]> fileArray = new ArrayList<>(); //name-path-size
    public final String FILE_TRENNER = "<>";
    private boolean makeIndex = false;
    private String[] suffix = {""};
    private boolean ohneSuffix = true;
    public static String MEDIA_DB_NAME = "Name";
    public static int MEDIA_DB_NAME_NR = 0;
    public static String MEDIA_DB_PATH = "Pfad";
    public static int MEDIA_DB_PATH_NR = 1;
    public static String MEDIA_DB_SIZE = "Größe [MB]";
    public static int MEDIA_DB_SIZE_NR = 2;

    public static TModel getModel() {
        return new TModel(new Object[][]{}, new String[]{MEDIA_DB_NAME, MEDIA_DB_PATH, MEDIA_DB_SIZE});
    }

    public MVMediaDB() {
    }

    public synchronized int getSizeFileArray() {
        return fileArray.size();
    }

    public synchronized void getModelMediaDB(TModel modelMediaDB) {
        modelMediaDB.setRowCount(0);
        for (String[] s : fileArray) {
            modelMediaDB.addRow(s);
        }
    }

    public synchronized void searchFiles(TModel modelFilm, String title) {

        modelFilm.setRowCount(0);
        if (!makeIndex && !title.isEmpty()) {
            Pattern p = makePattern(title);
            if (p != null) {
                // dann mit RegEx prüfen
                for (String[] s : fileArray) {
                    if (p.matcher(s[MEDIA_DB_NAME_NR]).matches()) {
                        modelFilm.addRow(s);
                    }
                }
            } else {
                title = title.toLowerCase();
                for (String[] s : fileArray) {
                    if (s[MEDIA_DB_NAME_NR].toLowerCase().contains(title)) {
                        modelFilm.addRow(s);
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
                    for (String s : db.split(FILE_TRENNER)) {
                        File f = new File(s);
                        searchFile(f);
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(120321254, ex);
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
                            fileArray.add(getItem(file.getName(), file.getParent().intern(), getGroesse(file.length())));
                        }
                    }
                }
            }
        }
    }

    private String[] getItem(String name, String path, String size) {
        return new String[]{name, path, size};
    }

    private String getGroesse(long l) {
        // l: Anzahl Bytes
        String ret = "";
        if (l > 1000 * 1000) {
            // größer als 1MB sonst kann ich mirs sparen
            ret = String.valueOf(l / (1000 * 1000));
        } else if (l > 0) {
            //0<....<1M
            ret = "< 1";
        } else if (l == 0) {
            ret = "0";
        }
        return ret;
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
            MSLog.systemMeldung("MediaDB schreiben (" + fileArray.size() + " Dateien) :");
            File file = new File(datei);
            File dir = new File(file.getParent());
            if (!dir.exists()) {
                if (!dir.mkdirs()) {
                    MSLog.fehlerMeldung(945120365, "Kann den Pfad nicht anlegen: " + dir.toString());
                }
            }
            MSLog.systemMeldung("   --> Start Schreiben nach: " + datei);
            out = new OutputStreamWriter(new FileOutputStream(datei), MSConst.KODIERUNG_UTF);

            for (String[] s : fileArray) {
                out.write(s[MEDIA_DB_NAME_NR] + "\n");
            }
            MSLog.systemMeldung("   --> geschrieben!");
        } catch (Exception ex) {
            MSLog.fehlerMeldung(102035478, ex, "nach: " + datei);
        } finally {
            try {
                if (out != null) {
                    out.close();
                }
            } catch (Exception e) {
            }
        }
    }

}
