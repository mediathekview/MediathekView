package mediathek.daten;

import mediathek.tool.GuiFunktionenProgramme;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;

import static mediathek.controller.starter.RuntimeExec.TRENNER_PROG_ARRAY;

public class DatenProg implements Comparable<DatenProg> {

    public static final int PROGRAMM_NAME = 0;
    public static final int PROGRAMM_ZIEL_DATEINAME = 1;
    public static final int PROGRAMM_PROGRAMMPFAD = 2;
    public static final int PROGRAMM_SCHALTER = 3;
    public static final int PROGRAMM_PRAEFIX = 4;
    public static final int PROGRAMM_SUFFIX = 5;
    public static final int PROGRAMM_RESTART = 6;
    public static final int PROGRAMM_DOWNLOADMANAGER = 7;

    public static final int MAX_ELEM = 8;
    public static final String TAG = "Programm";
    public static final String[] COLUMN_NAMES = {"Beschreibung", "Zieldateiname", "Programm",
        "Schalter", "Präfix", "Suffix", "Restart", "Downloadmanager"};
    public static final String[] XML_NAMES = {"Programmname", "Zieldateiname", "Programmpfad",
        "Programmschalter", "Praefix", "Suffix", "Restart", "Downloadmanager"};

    public String[] arr;

    public DatenProg() {
        initialize();
        arr[PROGRAMM_RESTART] = Boolean.toString(false);
        arr[PROGRAMM_DOWNLOADMANAGER] = Boolean.toString(false);
    }

    public DatenProg(String name, String programmpfad, String schalter, String restart, String downloadmanager) {
        initialize();
        arr[PROGRAMM_NAME] = name;
        arr[PROGRAMM_PROGRAMMPFAD] = programmpfad;
        arr[PROGRAMM_SCHALTER] = schalter;
        arr[PROGRAMM_RESTART] = restart.equals("") ? Boolean.toString(false) : restart;
        arr[PROGRAMM_DOWNLOADMANAGER] = downloadmanager.equals("") ? Boolean.toString(false) : downloadmanager;
    }

    public DatenProg copy() {
        DatenProg ret = new DatenProg();
        System.arraycopy(this.arr, 0, ret.arr, 0, arr.length);
        return ret;
    }

    public boolean isRestart() {
        if (arr[PROGRAMM_RESTART].equals("")) {
            return false;
        }
        return Boolean.parseBoolean(arr[PROGRAMM_RESTART]);
    }

    public boolean isDownloadManager() {
        if (arr[PROGRAMM_DOWNLOADMANAGER].equals("")) {
            return false;
        }
        return Boolean.parseBoolean(arr[PROGRAMM_DOWNLOADMANAGER]);
    }

    public boolean urlTesten(String url) {
        //prüfen ob das Programm zur Url passt
        boolean ret = false;
        if (url != null) {
            //Felder sind entweder leer oder passen
            if (GuiFunktionenProgramme.praefixTesten(this.arr[PROGRAMM_PRAEFIX], url, true)
                    && GuiFunktionenProgramme.praefixTesten(this.arr[PROGRAMM_SUFFIX], url, false)) {
                ret = true;
            }
        }
        return ret;
    }

    public String getProgrammAufruf() {
        return arr[PROGRAMM_PROGRAMMPFAD] + " " + arr[PROGRAMM_SCHALTER];
    }

    public String getProgrammAufrufArray() {
        String ret;
        ret = arr[DatenProg.PROGRAMM_PROGRAMMPFAD];
        String[] ar = arr[DatenProg.PROGRAMM_SCHALTER].split(" ");
        for (String s : ar) {
            ret = ret + TRENNER_PROG_ARRAY + s;
        }
        return ret;
    }

    public static String makeProgAufrufArray(String pArray) {
        String[] progArray = pArray.split(TRENNER_PROG_ARRAY);
        String execStr = "";
        for (String s : progArray) {
            execStr = execStr + s + " ";
        }
        execStr = execStr.trim(); // letztes Leerzeichen wieder entfernen
        return execStr;
    }

    @Override
    public String toString() {
        String ret = "";
        for (int i = 0; i < MAX_ELEM; ++i) {
            if (i == 0) {
                ret += "| ***|" + COLUMN_NAMES[i] + ": " + arr[i] + System.lineSeparator();
            } else {
                ret += "|    |" + COLUMN_NAMES[i] + ": " + arr[i] + System.lineSeparator();
            }
        }
        return ret;
    }

    private void initialize() {
        arr = new String[MAX_ELEM];
        Arrays.fill(arr,"");
    }

    @Override
    public int compareTo(@NotNull DatenProg o) {
        return 0;
    }
}
