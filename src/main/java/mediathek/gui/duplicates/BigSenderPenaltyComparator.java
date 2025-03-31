package mediathek.gui.duplicates;

import mediathek.daten.DatenFilm;

import java.util.Comparator;

public class BigSenderPenaltyComparator implements Comparator<DatenFilm> {
    @Override
    public int compare(DatenFilm s1, DatenFilm s2) {
        // "ARD" und "ZDF" immer am Ende um die kleineren Mediatheken nicht zu benachteiligen
        final var s1_sender = s1.getSender();
        final var s2_sender = s2.getSender();
        if (s1_sender.equals("ARD") || s1_sender.equals("ZDF")) {
            return 1;
        }
        if (s2_sender.equals("ARD") || s2_sender.equals("ZDF")) {
            return -1;
        }
        // Alphabetisch sortieren für alle anderen
        return s1.compareTo(s2);
    }
}
