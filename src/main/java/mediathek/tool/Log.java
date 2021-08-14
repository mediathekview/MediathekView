package mediathek.tool;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;

public class Log {

    public final static String LINE = "################################################################################";
    public static final LocalDateTime startZeit = LocalDateTime.now();
    private static final ArrayList<String> logList = new ArrayList<>();
    private static final Logger logger = LogManager.getLogger();
    private static boolean progress;

    /**
     * Output runtime statistics to console and log file
     */
    public static void printRuntimeStatistics() {
        DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;
        final var endZeit = LocalDateTime.now();
        final var runTime = LocalTime.MIN.plusSeconds(Duration.between(startZeit,endZeit).toSeconds());

        logger.info(LINE);
        logger.info("   --> Start: {}", formatter.format(startZeit));
        logger.info("   --> Ende:  {}", formatter.format(endZeit));
        logger.info("   --> Laufzeit: {}h {}m {}s", runTime.getHour(),runTime.getMinute(),runTime.getSecond());
        logger.info(LINE);
    }

    // Fehlermeldung mit Exceptions
    public static synchronized void sysLog(String text) {
        systemmeldung_(new String[]{text});
    }

    public static synchronized void progress(String texte) {
        progress = true;
        if (!texte.isEmpty()) {
            System.out.print(texte + '\r');
        }
    }

    private static void resetProgress() {
        // Leerzeile um die Progresszeile zu löschen
        if (progress) {
            System.out.print("                                                                                                             \r");
            progress = false;
        }
    }

    private static void systemmeldung_(String[] texte) {
        resetProgress();
        final String z = ". ";
        if (texte.length <= 1) {
            logList.add(z + ' ' + texte[0]);
        } else {
            String zeile = "---------------------------------------";
            String txt;
            logList.add(z + zeile);
            for (String aTexte : texte) {
                txt = "| " + aTexte;
                logList.add(z + txt);
            }
            logList.add(z + zeile);
        }
        printLog();
    }

    private static void printLog() {
        logList.forEach(System.out::println);
        logList.clear();
    }
}
