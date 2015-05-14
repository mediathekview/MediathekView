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
import java.util.ArrayList;
import java.util.regex.Pattern;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import static mediathek.tool.Filter.makePattern;

public class MVMediaDB {

    public final ArrayList<String[]> fileArray = new ArrayList<>(); //name-path
    public final String FILE_TRENNER = "<>";
    private boolean makeIndex = false;
    String[] suffix = {""};

    public MVMediaDB() {
    }

    public synchronized int getSizeFileArray() {
        return fileArray.size();
    }

    public synchronized void search(TModel modelFilm, String title) {

        modelFilm.setRowCount(0);
        if (!makeIndex && !title.isEmpty()) {
            Pattern p = makePattern(title);
            if (p != null) {
                // dann mit RegEx prüfen
                for (String[] s : fileArray) {
                    if (p.matcher(s[1]).matches()) {
                        modelFilm.addRow(s);
                    }
                }
            } else {
                title = title.toLowerCase();
                for (String[] s : fileArray) {
                    if (s[0].toLowerCase().contains(title)) {
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
            if (!suffix[i].startsWith(".")) {
                suffix[i] = "." + suffix[i];
            }
        }
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
                        if (!checkSuffix(suffix, file.getName())) {
                            fileArray.add(new String[]{file.getName(), file.getParent().intern()});
//                        } else {
//                            System.out.println("geht nicht: " + file.getName());
                        }
                    }
                }
            }
        }
    }

    public static boolean checkSuffix(String[] str, String uurl) {
        //prüfen ob url mit einem Argument in str endet
        //wenn str leer dann false
        boolean ret = false;
        final String url = uurl.toLowerCase();
        for (String s : str) {
            //Suffix prüfen
            if (!s.isEmpty() && url.endsWith(s)) {
                ret = true;
                break;
            }
        }
        return ret;
    }

}
