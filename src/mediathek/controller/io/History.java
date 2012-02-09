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

import java.io.*;
import java.util.HashSet;
import java.util.Iterator;

public class History extends HashSet<String> {

    public String datei = "";

    public History(String history_datei) {
        datei = history_datei;
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
                add(strLine);
            }
            //Close the input stream
            in.close();
        } catch (Exception e) {//Catch exception if any
            System.err.println("Fehler: " + e.getMessage());
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
            System.err.println("Fehler: " + e.getMessage());
        }
    }

    public void loschen() {
        this.clear();
        File file = new File(datei);
        if (!file.exists()) {
            return;
        }
        file.delete();
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
