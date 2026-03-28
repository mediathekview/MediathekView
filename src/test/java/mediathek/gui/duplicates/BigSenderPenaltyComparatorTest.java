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
import mediathek.tool.GermanStringSorter;
import mediathek.tool.SenderListBoxModel;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

class BigSenderPenaltyComparatorTest {

    @Test
    void sortProvidedSenderList_movesArdAndZdfToEnd() {
        var inputFilms = SenderListBoxModel.getProvidedSenderList().stream()
                .map(this::createFilmWithSender).sorted(new BigSenderPenaltyComparator()).collect(Collectors.toCollection(ArrayList::new));

        var sortedSenders = inputFilms.stream().map(DatenFilm::getSender).toList();
        int senderCount = sortedSenders.size();

        var senderSet = Set.of("ARD", "ZDF");
        assertTrue(senderSet.contains(sortedSenders.get(senderCount - 1)));
        assertTrue(senderSet.contains(sortedSenders.get(senderCount - 2)));
        assertNotEquals(sortedSenders.get(senderCount - 1), sortedSenders.get(senderCount - 2));

        var expectedWithoutPenalty = SenderListBoxModel.getProvidedSenderList().stream()
                .filter(sender -> !"ARD".equals(sender) && !"ZDF".equals(sender))
                .sorted(GermanStringSorter.getInstance())
                .toList();
        var actualWithoutPenalty = sortedSenders.subList(0, senderCount - 2);

        assertEquals(expectedWithoutPenalty, actualWithoutPenalty);
    }

    private DatenFilm createFilmWithSender(String sender) {
        var film = new DatenFilm();
        film.setSender(sender);
        return film;
    }
}
