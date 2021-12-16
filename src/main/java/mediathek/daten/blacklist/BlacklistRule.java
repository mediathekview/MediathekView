package mediathek.daten.blacklist;

import mediathek.tool.Filter;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;

public class BlacklistRule {

    public static final int MAX_ELEM = 5;
    public static final String TAG = "Blacklist";
    public static final String[] XML_NAMES = {"black-nr", "black-sender", "black-thema", "black-titel", "black-thema-titel"};
    private static final int BLACKLIST_NR = 0;
    private static final int BLACKLIST_THEMA_TITEL = 4;
    private static final int BLACKLIST_TITEL = 3;
    private static final int BLACKLIST_THEMA = 2;
    private static final int BLACKLIST_SENDER = 1;
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

    public @NotNull String getNr() {
        return arr[BLACKLIST_NR];
    }

    public void setNr(@NotNull String s) {
        arr[BLACKLIST_NR] = s;
    }

    public @NotNull String getThemaTitel() {
        return arr[BLACKLIST_THEMA_TITEL];
    }

    public void setThemaTitel(@NotNull String s) {
        arr[BLACKLIST_THEMA_TITEL] = s;
    }

    public @NotNull String getSender() {
        return arr[BLACKLIST_SENDER];
    }

    public void setSender(@NotNull String s) {
        arr[BLACKLIST_SENDER] = s;
    }

    public @NotNull String getThema() {
        return arr[BLACKLIST_THEMA];
    }

    public void setThema(@NotNull String s) {
        arr[BLACKLIST_THEMA] = s;
    }

    public @NotNull String getTitel() {
        return arr[BLACKLIST_TITEL];
    }

    public void setTitel(@NotNull String s) {
        arr[BLACKLIST_TITEL] = s;
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
}
