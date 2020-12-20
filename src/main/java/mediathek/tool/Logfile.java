package mediathek.tool;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.BufferedWriter;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;

public class Logfile {

    private static final Logger logger = LogManager.getLogger();

    public static boolean LogDateiSchreiben(String ziel, String progVersion, String settingsDir, ArrayList<String> progs, String[][] configs) {
        boolean ret;

        Path logFilePath = Paths.get(ziel);
        try (OutputStream os = Files.newOutputStream(logFilePath);
             OutputStreamWriter osw = new OutputStreamWriter(os);
             BufferedWriter bw = new BufferedWriter(osw)) {
            bw.write("Erstellt: " + DateTimeFormatter.ofPattern("dd.MM.yyyy, HH:mm").format(LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault())));
            bw.newLine();
            bw.write("#####################################################");
            bw.newLine();
            bw.write(progVersion);
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
            bw.write("Programmpfad: " + MVFunctionSys.getPathToApplicationJar());
            bw.newLine();
            bw.write("Verzeichnis Einstellungen: " + settingsDir);
            bw.newLine();
            bw.write("=====================================================");
            bw.newLine();
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
            logger.error("Error writing log file",ex);
            ret = false;
        }
        return ret;
    }

}
