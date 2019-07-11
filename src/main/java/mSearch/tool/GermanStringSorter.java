package mSearch.tool;

import org.jetbrains.annotations.NotNull;

import java.text.Collator;
import java.util.Comparator;
import java.util.Locale;

public class GermanStringSorter implements Comparator<String> {

    private static final Collator collator = Collator.getInstance(Locale.GERMANY);
    private static GermanStringSorter instance;

    private GermanStringSorter() {
        super();
    }

    public static GermanStringSorter getInstance() {
        if (instance == null) {
            instance = new GermanStringSorter();
            // ignore lower/upper case, but accept special characters in localised alphabetical order
            collator.setStrength(Collator.SECONDARY);
        }
        return instance;
    }

    @Override
    public int compare(@NotNull String o1, @NotNull String o2) {
        return collator.compare(o1, o2);
    }
}
