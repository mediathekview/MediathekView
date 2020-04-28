package mediathek.tool;

import mediathek.config.Config;
import mediathek.config.Konstanten;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;

public class Log {

    public final static String LINE = "################################################################################";
    public static final Instant startZeit = Instant.now();
    private final static String FEHLER = "Fehler(" + Konstanten.PROGRAMMNAME + "): ";
    private static final ArrayList<String> logList = new ArrayList<>();
    private static final Logger logger = LogManager.getLogger();
    private static boolean progress;

    /**
     * Output runtime statistics to console and log file
     */
    public static void printRuntimeStatistics() {
        DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;
        var systemZone = ZoneId.systemDefault();
        var endZeit = Instant.now();
        long runtimeDuration = Duration.between(startZeit,endZeit).toSeconds();
        var str = LocalTime.MIN.plusSeconds(runtimeDuration);

        logger.info(LINE);
        logger.info("   --> Start: {}", formatter.format(LocalDateTime.ofInstant(startZeit, systemZone)));
        logger.info("   --> Ende: {}", formatter.format(LocalDateTime.ofInstant(endZeit, systemZone)));
        logger.info("   --> Laufzeit: {}h {}m {}s", str.getHour(),str.getMinute(),str.getSecond());
        logger.info(LINE);
    }

    // Fehlermeldung mit Exceptions
    @Deprecated
    public static synchronized void errorLog(int fehlerNummer, Exception ex) {
        fehlermeldung_(fehlerNummer, ex, new String[]{});
    }

    @Deprecated
    public static synchronized void errorLog(int fehlerNummer, Exception ex, String text) {
        fehlermeldung_(fehlerNummer, ex, new String[]{text});
    }

    @Deprecated
    public static synchronized void errorLog(int fehlerNummer, Exception ex, String[] text) {
        fehlermeldung_(fehlerNummer, ex, text);
    }

    // Fehlermeldungen
    @Deprecated
    public static synchronized void errorLog(int fehlerNummer, String text) {
        fehlermeldung_(fehlerNummer, null, new String[]{text});
    }

    @Deprecated
    public static synchronized void errorLog(int fehlerNummer, String[] text) {
        fehlermeldung_(fehlerNummer, null, text);
    }

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

    private static void fehlermeldung_(int fehlerNummer, Exception ex, String[] texte) {
        final Throwable t = new Throwable();
        final StackTraceElement methodCaller = t.getStackTrace()[2];
        final String klasse = methodCaller.getClassName() + '.' + methodCaller.getMethodName();
        String kl;
        try {
            kl = klasse;
            while (kl.contains(".")) {
                if (Character.isUpperCase(kl.charAt(0))) {
                    break;
                } else {
                    kl = kl.substring(kl.indexOf('.') + 1);
                }
            }
        } catch (Exception ignored) {
            kl = klasse;
        }

        if (ex != null || Config.isDebugModeEnabled()) {
            // Exceptions immer ausgeben
            resetProgress();
            String x, z;
            if (ex != null) {
                x = "!";
            } else {
                x = "=";
            }
            z = "*";
            logList.add(x + x + x + x + x + x + x + x + x + x
                    + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x);

            try {
                // Stacktrace
                try (StringWriter sw = new StringWriter(); PrintWriter pw = new PrintWriter(sw)) {
                    if (ex != null) {
                        ex.printStackTrace(pw);
                    }
                    pw.flush();
                    sw.flush();
                    logList.add(sw.toString());
                }
            } catch (Exception ignored) {
            }

            logList.add(z + " Fehlernr: " + fehlerNummer);
            if (ex != null) {
                logList.add(z + " Exception: " + ex.getMessage());
            }
            logList.add(z + ' ' + FEHLER + kl);
            for (String aTexte : texte) {
                logList.add(z + "           " + aTexte);
            }
            logList.add("");
            printLog();
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
