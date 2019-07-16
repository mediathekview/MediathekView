package mediathek.daten;

import mediathek.tool.Filter;

import java.util.Arrays;

public class DatenBlacklist extends MVData<DatenBlacklist> {

    public static final int BLACKLIST_NR = 0;
    public static final int BLACKLIST_SENDER = 1;
    public static final int BLACKLIST_THEMA = 2;
    public static final int BLACKLIST_TITEL = 3;
    public static final int BLACKLIST_THEMA_TITEL = 4;

    public static final int MAX_ELEM = 5;
    public static final String TAG = "Blacklist";
    public static final String[] COLUMN_NAMES = {"Nr", "Sender", "Thema", "Titel", "Thema-Titel"};
    public static final String[] XML_NAMES = {"black-nr", "black-sender", "black-thema", "black-titel", "black-thema-titel"};
    public boolean patternTitle = true;
    public boolean patternThema = true;

    public String[] arr;

    public DatenBlacklist() {
        initialize();
    }

    public DatenBlacklist(String sender, String thema, String titel, String themaTitel) {
        initialize();
        arr[BLACKLIST_NR] = "";
        arr[BLACKLIST_SENDER] = sender;
        arr[BLACKLIST_THEMA] = thema;
        arr[BLACKLIST_TITEL] = titel;
        arr[BLACKLIST_THEMA_TITEL] = themaTitel;
    }

    public void hasPattern() {
        patternTitle = Filter.isPattern(arr[BLACKLIST_TITEL]);
        patternThema = Filter.isPattern(arr[BLACKLIST_THEMA_TITEL]);
    }

    public void toLower() {
        arr[BLACKLIST_TITEL] = arr[BLACKLIST_TITEL].toLowerCase();
        arr[BLACKLIST_THEMA_TITEL] = arr[BLACKLIST_THEMA_TITEL].toLowerCase();
    }
    private void initialize() {
        arr = new String[MAX_ELEM];
        Arrays.fill(arr,"");
    }
}
