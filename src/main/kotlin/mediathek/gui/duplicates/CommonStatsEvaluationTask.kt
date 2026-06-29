package mediathek.gui.duplicates

import mediathek.filmlisten.FilmCatalog

class CommonStatsEvaluationTask(
    private val filmCatalog: FilmCatalog,
) : Runnable {
    override fun run() {
        val statisticsMap = filmCatalog.allFilms.parallelStream()
            .filter { film -> !film.isLivestream }
            .countFilmsBySender()

        replaceFilmStatistics(filmCatalog.commonStatistics, statisticsMap)
    }
}
