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

import ca.odell.glazedlists.TransactionList;
import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class FilmDuplicateEvaluationTask implements Runnable {
    private static final Logger logger = LogManager.getLogger();
    private final ListeFilme listeFilme;

    public FilmDuplicateEvaluationTask() {
        this.listeFilme = Daten.getInstance().getListeFilme();
    }

    private void printDuplicateStatistics() {
        var statisticsEventList = Daten.getInstance().getDuplicateStatistics();
        Map<String, Long> statisticsMap = listeFilme.parallelStream()
                .filter(DatenFilm::isDuplicate)
                .collect(Collectors.groupingBy(DatenFilm::getSender, Collectors.counting()));
        long duplicateCount = statisticsMap.values().stream().mapToLong(Long::longValue).sum();
        TransactionList<FilmStatistics> tList = new TransactionList<>(statisticsEventList);
        tList.getReadWriteLock().writeLock().lock();
        try {
            tList.beginEvent(true);
            tList.clear();
            for (var sender : statisticsMap.keySet()) {
                tList.add(new FilmStatistics(sender, statisticsMap.get(sender)));
            }
            tList.commitEvent();
        }
        finally {
            tList.getReadWriteLock().writeLock().unlock();
        }
        logger.trace("Number of duplicates: {}", duplicateCount);
    }

    private void checkDuplicates() {
        logger.trace("Start Duplicate URL search");
        final Map<String, Map<String, Set<String>>> urlCache = new HashMap<>();
        listeFilme.stream()
                .filter(f -> !f.isLivestream())
                .sorted(new BigSenderPenaltyComparator())
                .forEach(film -> {
                    final String normalUrl = film.getUrlNormalQuality();
                    final String highQualityUrl = film.isHighQuality() ? film.getHighQualityUrl() : "";
                    final String lowQualityUrl = film.hasLowQuality() ? film.getLowQualityUrl() : "";

                    Map<String, Set<String>> byHighQualityUrl = urlCache.computeIfAbsent(normalUrl, _ -> new HashMap<>());
                    Set<String> seenLowQualityUrls = byHighQualityUrl.computeIfAbsent(highQualityUrl, _ -> new HashSet<>());

                    film.setDuplicate(!seenLowQualityUrls.add(lowQualityUrl));
                });
        urlCache.clear();
    }

    @Override
    public void run() {
        checkDuplicates();
        printDuplicateStatistics();
    }
}
