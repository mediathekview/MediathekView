/*
 *    MediathekView
 *    Copyright (C) 2010 by Andreas M.
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
package mediathek.controller.io;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.HashSet;
import java.util.Iterator;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class History extends HashSet<String> {

    private String datei = "";

    public History(String history_datei) {
        datei = history_datei;
    }

    @Override
    public boolean add(String url) {
        boolean ret = super.add(url);
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_HISTORY_GEAENDERT, History.class.getSimpleName());
        return ret;
    }

    public void add(String[] url) {
        for (String s : url) {
            super.add(s);
        }
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_HISTORY_GEAENDERT, History.class.getSimpleName());
    }

    @Override
    public boolean remove(Object url) {
        boolean ret = super.remove(url);
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_HISTORY_GEAENDERT, History.class.getSimpleName());
        return ret;
    }

    public void laden() {
        clear();
        File file = new File(datei);
        if (!file.exists()) {
            return;
        }
        try {
            FileInputStream fstream = new FileInputStream(datei);
            DataInputStream in = new DataInputStream(fstream);
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            String strLine;
            while ((strLine = br.readLine()) != null) {
                super.add(strLine);
            }
            //Close the input stream
            in.close();
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_HISTORY_GEAENDERT, History.class.getSimpleName());
        } catch (Exception e) {//Catch exception if any
            System.err.println("Fehler: " + e);
            Log.fehlerMeldung(303049876, Log.FEHLER_ART_PROG, History.class.getName(), e);
        }
    }

    public void speichern() {
        try {
            FileOutputStream fstream = new FileOutputStream(datei);
            DataOutputStream out = new DataOutputStream(fstream);
            BufferedWriter br = new BufferedWriter(new OutputStreamWriter(out));
            for (String h : this) {
                br.write(h + "\n");
            }
            br.flush();
            out.close();
        } catch (Exception e) {//Catch exception if any
            Log.fehlerMeldung(978786563, Log.FEHLER_ART_PROG, History.class.getName(), e);
        }
    }

    public void loschen() {
        this.clear();
        File file = new File(datei);
        if (!file.exists()) {
            return;
        }
        file.delete();
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_HISTORY_GEAENDERT, History.class.getSimpleName());
    }

    public Object[][] getObjectData() {
        Object[][] object;
        int i = 0;
        Iterator<String> iterator = this.iterator();
        object = new Object[this.size()][1];
        while (iterator.hasNext()) {
            object[i][0] = iterator.next();
            ++i;
        }
        return object;
    }
}
