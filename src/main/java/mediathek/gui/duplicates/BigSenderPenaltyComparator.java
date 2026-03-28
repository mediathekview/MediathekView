/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.duplicates;

import mediathek.daten.DatenFilm;

import java.util.Comparator;
import java.util.Set;

public class BigSenderPenaltyComparator implements Comparator<DatenFilm> {
    private static final Set<String> PENALIZED_SENDERS = Set.of("ARD", "ZDF");

    @Override
    public int compare(DatenFilm s1, DatenFilm s2) {
        // "ARD" und "ZDF" immer am Ende um die kleineren Mediatheken nicht zu benachteiligen
        int s1Penalty = penalty(s1.getSender());
        int s2Penalty = penalty(s2.getSender());
        if (s1Penalty != s2Penalty) {
            return Integer.compare(s1Penalty, s2Penalty);
        }

        // Alphabetisch sortieren für alle anderen
        return s1.compareTo(s2);
    }

    private static int penalty(String sender) {
        return PENALIZED_SENDERS.contains(sender) ? 1 : 0;
    }
}
