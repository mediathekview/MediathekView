package mediathek.daten.blacklist;

import mediathek.tool.Filter;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;

public class BlacklistRule implements Comparable<BlacklistRule> {

    public static final int BLACKLIST_NR = 0;
    public static final int BLACKLIST_SENDER = 1;
    public static final int BLACKLIST_THEMA = 2;
    public static final int BLACKLIST_TITEL = 3;
    public static final int BLACKLIST_THEMA_TITEL = 4;

    public static final int MAX_ELEM = 5;
    public static final String TAG = "Blacklist";
    public static final String[] COLUMN_NAMES = {"Nr", "Sender", "Thema", "Titel", "Thema-Titel"};
    public static final String[] XML_NAMES = {"black-nr", "black-sender", "black-thema", "black-titel", "black-thema-titel"};
    public String[] arr;
    private boolean patternTitle = true;
    private boolean patternThema = true;

    public BlacklistRule() {
        arr = new String[MAX_ELEM];
        Arrays.fill(arr, "");
    }

    public BlacklistRule(String sender, String thema, String titel, String themaTitel) {
        this();

        arr[BLACKLIST_SENDER] = sender;
        arr[BLACKLIST_THEMA] = thema;
        arr[BLACKLIST_TITEL] = titel;
        arr[BLACKLIST_THEMA_TITEL] = themaTitel;
    }

    /**
     * Determine if we have regexp patterns somewhere and also precompile the pattern into the cache to speed up
     * operations a bit.
     */
    public void checkPatterns() {
        patternTitle = Filter.isPattern(arr[BLACKLIST_TITEL]);
        patternThema = Filter.isPattern(arr[BLACKLIST_THEMA_TITEL]);

        //precompile and cache the regexp patterns if needed...
        if (patternTitle)
            Filter.makePattern(arr[BLACKLIST_TITEL]);

        if (patternThema)
            Filter.makePattern(arr[BLACKLIST_THEMA_TITEL]);
    }

    public boolean hasTitlePattern() {
        return patternTitle;
    }

    public boolean hasThemaPattern() {
        return patternThema;
    }

    public void convertToLowerCase() {
        arr[BLACKLIST_TITEL] = arr[BLACKLIST_TITEL].toLowerCase();
        arr[BLACKLIST_THEMA_TITEL] = arr[BLACKLIST_THEMA_TITEL].toLowerCase();
    }

    @Override
    public int compareTo(@NotNull BlacklistRule o) {
        return 0;
    }
}
