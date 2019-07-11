package mediathek.tool;

import mSearch.tool.Functions;
import mSearch.tool.Log;

import java.io.BufferedWriter;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

public class Logfile {

    public static boolean LogDateiSchreiben(String ziel, String progVersion, String settingsDir, ArrayList<String> progs, String[][] configs) {
        boolean ret;

        Path logFilePath = Paths.get(ziel);
        try (OutputStream os = Files.newOutputStream(logFilePath);
             OutputStreamWriter osw = new OutputStreamWriter(os);
             BufferedWriter bw = new BufferedWriter(osw)) {
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
            bw.write("Compiled: " + Functions.getCompileDate());
            bw.newLine();
            bw.newLine();
            bw.write("=====================================================");
            bw.newLine();
            bw.write("Java");
            bw.newLine();
            String[] java = Functions.getJavaVersion();
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
            bw.write("Programmpfad: " + Functions.getPathJar());
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

            ret = true;
        } catch (Exception ex) {
            Log.errorLog(319865493, ex);
            ret = false;
        }
        return ret;
    }

}
