package mediathek.config;

import java.util.concurrent.TimeUnit;

public class Const {

    // Dateien/Verzeichnisse
    public static final String FORMAT_ZIP = ".zip";
    public static final String FORMAT_XZ = ".xz";

    public static final long ALTER_FILMLISTE_SEKUNDEN_FUER_AUTOUPDATE = TimeUnit.SECONDS.convert(3, TimeUnit.HOURS);
    public static final String TIME_MAX_AGE_FOR_DIFF = "09"; // Uhrzeit ab der die Diffliste alle Änderungen abdeckt, die Filmliste darf also nicht vor xx erstellt worden sein
}
