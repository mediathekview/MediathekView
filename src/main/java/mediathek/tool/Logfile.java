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

import mSearch.tool.Log;
import mSearch.tool.SysMsg;

import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

import static mSearch.tool.Functions.*;

public class Logfile {

    public static boolean LogDateiSchreiben(String ziel, String progVersion, String settingsDir, ArrayList<String> progs, String[][] configs) {
        boolean ret;

        ArrayList<String> retList;
        Path logFilePath = Paths.get(ziel);
        try (final BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(logFilePath)))) {
            bw.write("");
            bw.newLine();

            //  ___  ___         _ _       _   _          _    _   _ _               
            //  |  \/  |        | (_)     | | | |        | |  | | | (_)              
            //  | .  . | ___  __| |_  __ _| |_| |__   ___| | _| | | |_  _____      __
            //  | |\/| |/ _ \/ _` | |/ _` | __| '_ \ / _ \ |/ / | | | |/ _ \ \ /\ / /
            //  | |  | |  __/ (_| | | (_| | |_| | | |  __/   <\ \_/ / |  __/\ V  V / 
            //  \_|  |_/\___|\__,_|_|\__,_|\__|_| |_|\___|_|\_\\___/|_|\___| \_/\_/  
            bw.write("___  ___         _ _       _   _          _    _   _ _               ");
            bw.newLine();
            bw.write("|  \\/  |        | (_)     | | | |        | |  | | | (_)              ");
            bw.newLine();
            bw.write("| .  . | ___  __| |_  __ _| |_| |__   ___| | _| | | |_  _____      __");
            bw.newLine();
            bw.write("| |\\/| |/ _ \\/ _` | |/ _` | __| '_ \\ / _ \\ |/ / | | | |/ _ \\ \\ /\\ / /");
            bw.newLine();
            bw.write("| |  | |  __/ (_| | | (_| | |_| | | |  __/   <\\ \\_/ / |  __/\\ V  V / ");
            bw.newLine();
            bw.write("\\_|  |_/\\___|\\__,_|_|\\__,_|\\__|_| |_|\\___|_|\\_\\\\___/|_|\\___| \\_/\\_/  ");
            bw.newLine();
            bw.write("");
            bw.newLine();
            bw.write("");
            bw.newLine();

            bw.write("#####################################################");
            bw.newLine();
            bw.write("Erstellt: " + new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(new Date()));
            bw.newLine();
            bw.write("#####################################################");
            bw.newLine();
            bw.newLine();
            bw.write(progVersion);
            bw.newLine();
            bw.write("Compiled: " + getCompileDate());
            bw.newLine();
            bw.newLine();
            bw.write("=====================================================");
            bw.newLine();
            bw.write("Java");
            bw.newLine();
            String[] java = getJavaVersion();
            for (String ja : java) {
                bw.write(ja);
                bw.newLine();
            }
            bw.newLine();
            bw.write("=====================================================");
            bw.newLine();
            bw.write("Betriebssystem: " + System.getProperty("os.name"));
            bw.newLine();
            bw.write("Bs-Version:     " + System.getProperty("os.version"));
            bw.newLine();
            bw.write("Bs-Architektur: " + System.getProperty("os.arch"));
            bw.newLine();
            bw.newLine();
            bw.write("=====================================================");
            bw.newLine();
            bw.write("Programmpfad: " + getPathJar());
            bw.newLine();
            bw.write("Verzeichnis Einstellungen: " + settingsDir);
            bw.newLine();
            bw.newLine();
            bw.write("=====================================================");
            bw.newLine();
            long totalMem = Runtime.getRuntime().totalMemory();
            bw.write("totalMemory: " + totalMem / (1000L * 1000L) + " MB");
            bw.newLine();
            long maxMem = Runtime.getRuntime().maxMemory();
            bw.write("maxMemory: " + maxMem / (1000L * 1000L) + " MB");
            bw.newLine();
            long freeMem = Runtime.getRuntime().freeMemory();
            bw.write("freeMemory: " + freeMem / (1000L * 1000L) + " MB");
            bw.newLine();
            bw.newLine();
            bw.newLine();

            //
            bw.write("#####################################################");
            bw.newLine();
            bw.write("## Programmeinstellungen ##########################");
            bw.newLine();
            bw.write("#####################################################");
            bw.newLine();
            bw.newLine();
            for (String[] s : configs) {
                if (!s[1].isEmpty()) {
                    bw.write(s[0] + '\t' + s[1]);
                    bw.newLine();
                }
            }
            bw.newLine();
            bw.newLine();
            bw.newLine();
            bw.newLine();
            //
            bw.write("#####################################################");
            bw.newLine();
            bw.write("## Programmsets ##################################");
            bw.newLine();
            bw.write("#####################################################");
            bw.newLine();
            bw.newLine();
            for (String s : progs) {
                bw.write(s);
                bw.newLine();
            }
            bw.newLine();
            bw.newLine();
            bw.newLine();
            bw.newLine();
            //
            bw.write("#####################################################");
            bw.newLine();
            bw.write("## Systemmeldungen ##################################");
            bw.newLine();
            bw.write("#####################################################");
            bw.newLine();
            bw.newLine();
            bw.write(SysMsg.textSystem.toString());
            bw.newLine();
            bw.newLine();
            bw.newLine();
            bw.newLine();
            //
            bw.write("#####################################################");
            bw.newLine();
            bw.write("## Programmausgabe ##################################");
            bw.newLine();
            bw.write("#####################################################");
            bw.newLine();
            bw.newLine();
            bw.write(SysMsg.textProgramm.toString());
            bw.newLine();
            bw.newLine();
            bw.newLine();
            bw.newLine();
            //
            bw.write("#########################################################");
            bw.newLine();
            bw.write("## Fehlermeldungen                                       ");
            bw.newLine();
            retList = Log.printErrorMsg();
            for (String s : retList) {
                bw.write(s);
                bw.newLine();
            }
            bw.newLine();
            bw.newLine();
            //
            ret = true;
        } catch (Exception ex) {
            Log.errorLog(319865493, ex);
            ret = false;
        }
        return ret;
    }

}
