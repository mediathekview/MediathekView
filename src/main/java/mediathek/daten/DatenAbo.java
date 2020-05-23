package mediathek.daten;

import mediathek.tool.GermanStringSorter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.Date;

public class DatenAbo implements Comparable<DatenAbo> {

    public static final int ABO_NR = 0;
    public static final int ABO_EINGESCHALTET = 1;
    public static final int ABO_NAME = 2;
    public static final int ABO_SENDER = 3;
    public static final int ABO_THEMA = 4;
    public static final int ABO_TITEL = 5;
    public static final int ABO_THEMA_TITEL = 6;
    public static final int ABO_IRGENDWO = 7;
    public static final int ABO_MINDESTDAUER = 8;
    public static final int ABO_MIN = 9;
    public static final int ABO_ZIELPFAD = 10;
    public static final int ABO_DOWN_DATUM = 11;
    public static final int ABO_PSET = 12;
    public static final String[] COLUMN_NAMES = {"Nr", "aktiv", "Name",
            "Sender", "Thema", "Titel", "Thema-Titel",
            "Irgendwo", "Dauer", "min/max", "Zielpfad", "letztes Abo", "Programmset"};
    public static final String[] XML_NAMES = {"Nr", "aktiv", "Name",
            "Sender", "Thema", "Titel", "Thema-Titel",
            "Irgendwo", "Mindestdauer", "min_max", "Zielpfad", "letztes_Abo", "Programmset"};

    public static final int MAX_ELEM = 13;
    public static final String TAG = "Abonnement";
    public static final String ABO_TARGET_BOOKMARK = "Merken";
    private static final Logger logger = LogManager.getLogger(DatenAbo.class);
    public static boolean[] spaltenAnzeigen = new boolean[MAX_ELEM];
    private final GermanStringSorter sorter = GermanStringSorter.getInstance();
    public int mindestdauerMinuten;
    public boolean min = true;
    public String[] arr;
    public int nr;
    private Date lastCheckDate;
    public String[] titel, thema, irgendwo;
    
    public DatenAbo() {
        initialize();
    }

    public DatenAbo(String name, String sender, String thema, String titel, String themaTitel, String irgendwo, int mmindestdauerMinuten, boolean min, String ziel, String pset) {
        initialize();
        arr[ABO_NAME] = name;
        arr[ABO_SENDER] = sender;
        arr[ABO_THEMA] = thema;
        arr[ABO_TITEL] = titel;
        arr[ABO_THEMA_TITEL] = themaTitel;
        arr[ABO_IRGENDWO] = irgendwo;
        setMindestDauerMinuten(mmindestdauerMinuten);
        arr[ABO_MIN] = Boolean.toString(min);
        this.min = min;
        arr[ABO_ZIELPFAD] = ziel;
        arr[ABO_PSET] = pset;
    }

    public static boolean anzeigen(int i) {
        return spaltenAnzeigen == null || spaltenAnzeigen[i];
    }

    public DatenAbo getCopy() {
        DatenAbo ret = new DatenAbo();
        System.arraycopy(this.arr, 0, ret.arr, 0, arr.length);
        ret.mindestdauerMinuten = this.mindestdauerMinuten;
        ret.min = this.min;
        return ret;
    }

    public boolean isEmpty() {
        //liefert TRUE wenn das Abo leer ist, also bei jedem Film ansprechen würde
        //ist dann offensichtlich falsch!!
        return arr[ABO_SENDER].isEmpty()
                && arr[ABO_THEMA].isEmpty()
                && arr[ABO_TITEL].isEmpty()
                && arr[ABO_THEMA_TITEL].isEmpty()
                && arr[ABO_IRGENDWO].isEmpty();
    }

    public final void setMindestDauerMinuten(int d) {
        mindestdauerMinuten = d;
        arr[ABO_MINDESTDAUER] = String.valueOf(d);
    }

    public void setMindestDauerMinuten() {
        if (this.arr[DatenAbo.ABO_MINDESTDAUER].isEmpty()) {
            // für den ProgUpdate
            mindestdauerMinuten = 0;
            arr[ABO_MINDESTDAUER] = "0";
        }
        try {
            mindestdauerMinuten = Integer.parseInt(this.arr[DatenAbo.ABO_MINDESTDAUER]);
        } catch (Exception ex) {
            logger.error("setMindestDauerMinuten()", ex);
            mindestdauerMinuten = 0;
            arr[ABO_MINDESTDAUER] = "0";
        }
    }

    public boolean aboIstEingeschaltet() {
        if (arr[DatenAbo.ABO_EINGESCHALTET].isEmpty()) {
            aboEin();
            return true;
        }
        return Boolean.parseBoolean(arr[DatenAbo.ABO_EINGESCHALTET]);
    }
    
    public void setNewAboState(boolean newstate) {
      arr[ABO_EINGESCHALTET] = String.valueOf(newstate);
      if (!newstate && isAboTargetBookmark()) { // if deactivated reset download date to force new scan after activation 
        arr[ABO_DOWN_DATUM] = "";
      }
    }
    
    public boolean isAboTargetBookmark() {
      return this.arr[ABO_PSET].equals(ABO_TARGET_BOOKMARK);
    }

    private void aboEin() {
        arr[DatenAbo.ABO_EINGESCHALTET] = String.valueOf(true);
    }

    private void initialize() {
        arr = new String[MAX_ELEM];
        Arrays.fill(arr, "");
        // neue Abos sind immer ein
        aboEin();
    }

    @Override
    public int compareTo(@NotNull DatenAbo arg0) {
        return sorter.compare(arr[ABO_NAME], arg0.arr[ABO_NAME]);
    }
    
    /**
     * Init variable from array field ABO_DOWN_DATUM, needed for bookmark abo operation
     */
    public void setLastCheckdate(Date lcd) {
      lastCheckDate = lcd;
    }
    
    public Date getLastCheckDate() {
      return lastCheckDate;
    }
}
