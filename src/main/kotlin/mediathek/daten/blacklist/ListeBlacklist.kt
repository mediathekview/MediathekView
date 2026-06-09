package mediathek.daten.blacklist

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Deferred
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.runBlocking
import mediathek.config.Daten
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.Country
import mediathek.daten.DatenFilm
import mediathek.daten.IndexedFilmList
import mediathek.gui.messages.BlacklistChangedEvent
import mediathek.gui.tabs.tab_film.filter.ZeitraumSpinner
import mediathek.mainwindow.MediathekGui
import mediathek.tool.MessageBus
import java.util.function.Predicate
import kotlin.time.Duration.Companion.days

class ListeBlacklist : ArrayList<BlacklistRule>() {
    private val geoblockingPredicate = GeoblockingPredicate()

    /**
     * This specifies the lower boundary for all films to be shown or not.
     * Content is num of days converted to milliseconds from UNIX start.
     */
    private var daysLowerBoundary = 0L
    private var doNotShowFutureFilms = false
    private var doNotShowGeoBlockedFilms = false
    private var blacklistIsActive = false

    /**
     * The minimum length in minutes a film should have.
     * Configuration in Settings/Blacklist panel.
     */
    private var minimumFilmLength = 0L

    /**
     * Add item without notifying registered listeners.
     */
    @Synchronized
    fun addWithoutNotification(rule: BlacklistRule) {
        super.add(rule)
    }

    @Synchronized
    override fun add(element: BlacklistRule): Boolean {
        val result = super.add(element)
        filterListAndNotifyListeners()
        return result
    }

    @Synchronized
    override fun remove(element: BlacklistRule): Boolean {
        val result = super.remove(element)
        filterListAndNotifyListeners()
        return result
    }

    /**
     * Remove a list of rules and filter after all objects have been removed.
     */
    @Synchronized
    fun remove(ruleList: List<BlacklistRule>) {
        ruleList.forEach { super.remove(it) }
        filterListAndNotifyListeners()
    }

    @Synchronized
    override fun removeAt(index: Int): BlacklistRule {
        val result = super.removeAt(index)
        filterListAndNotifyListeners()
        return result
    }

    @Synchronized
    override fun get(index: Int): BlacklistRule =
        super.get(index)

    @Synchronized
    override fun clear() {
        super.clear()
        filterListAndNotifyListeners()
    }

    /**
     * Main filtering routine.
     */
    @Synchronized
    fun filterListe() {
        val daten = Daten.getInstance()
        val completeFilmList = daten.listeFilme
        val filteredList = daten.listeFilmeNachBlackList

        filteredList.clear()
        loadCurrentFilterSettings()

        if (completeFilmList.isEmpty()) {
            return
        }

        filteredList.metaData = completeFilmList.metaData

        val applicationConfiguration = ApplicationConfiguration.getInstance()
        val evaluateDuplicates = applicationConfiguration.evaluateFilmDuplicates
        val filterBlacklistDuplicates = applicationConfiguration.isBlacklistDuplicateFilteringEnabled
        val filterDuplicates = evaluateDuplicates && filterBlacklistDuplicates

        val blacklistSnapshot = toList()
        val filmSnapshot = synchronized(completeFilmList) {
            completeFilmList.toList()
        }
        val predicate = createPredicate(blacklistSnapshot)

        val filteredSnapshot = filterFilmSnapshot(filmSnapshot, filterDuplicates, predicate)
        var index = 0
        while (index < filteredSnapshot.size) {
            filteredList.add(filteredSnapshot[index])
            index++
        }
    }

    /**
     * Setup dynamically the list of filter to be applied to blacklist film list.
     */
    private fun createPredicate(blacklistSnapshot: List<BlacklistRule>): Predicate<DatenFilm> {
        val filters = mutableListOf<(DatenFilm) -> Boolean>()

        // Keep it for the old-style search. It is useless for Lucene.
        if (Daten.getInstance().listeFilmeNachBlackList !is IndexedFilmList && daysLowerBoundary != 0L) {
            filters.add(::checkDate)
        }

        if (blacklistIsActive) {
            if (doNotShowGeoBlockedFilms) {
                filters.add(geoblockingPredicate::test)
            }
            if (doNotShowFutureFilms) {
                filters.add(::checkIfFilmIsInFuture)
            }
            if (minimumFilmLength != 0L) {
                filters.add(::checkFilmLength)
            }
            if (blacklistSnapshot.isNotEmpty()) {
                filters.add(ApplyBlacklistFilterPredicate(blacklistSnapshot)::test)
            }
        }

        return Predicate { film ->
            var index = 0
            while (index < filters.size) {
                if (!filters[index](film)) {
                    return@Predicate false
                }
                index++
            }
            true
        }
    }

    /**
     * Create a reusable blacklist predicate for Abo/download searches.
     */
    @Synchronized
    fun createDownloadsPredicate(): Predicate<DatenFilm> {
        loadCurrentFilterSettings()

        val filterGeoBlockedFilms = doNotShowGeoBlockedFilms
        val filterFutureFilms = doNotShowFutureFilms
        val minimumLength = minimumFilmLength
        val downloadsGeoblockingPredicate = GeoblockingPredicate()
        val blacklistPredicate: Predicate<DatenFilm> =
            if (isEmpty()) Predicate { true } else ApplyBlacklistFilterPredicate(toList())

        return Predicate { film ->
            if (filterGeoBlockedFilms && !downloadsGeoblockingPredicate.test(film)) {
                return@Predicate false
            }
            if (filterFutureFilms && !checkIfFilmIsInFuture(film)) {
                return@Predicate false
            }
            if (minimumLength != 0L && !checkFilmLength(film, minimumLength)) {
                // wegen der Möglichkeit "Whiteliste" muss das extra geprüft werden
                return@Predicate false
            }

            blacklistPredicate.test(film)
        }
    }

    /**
     * Filter the list and notify all registered listeners.
     */
    @Synchronized
    fun filterListAndNotifyListeners() {
        filterListe()
        MessageBus.messageBus.publishAsync(BlacklistChangedEvent())
    }

    private fun filterFilmSnapshot(
        filmSnapshot: List<DatenFilm>,
        filterDuplicates: Boolean,
        predicate: Predicate<DatenFilm>
    ): List<DatenFilm> {
        if (filmSnapshot.size < COROUTINE_FILTER_THRESHOLD) {
            return filterFilmRange(filmSnapshot, 0, filmSnapshot.size, filterDuplicates, predicate)
        }

        return runBlocking {
            val workerCount = Runtime.getRuntime().availableProcessors().coerceAtLeast(1)
            val chunkSize = (filmSnapshot.size / (workerCount * CHUNKS_PER_WORKER)).coerceAtLeast(MIN_FILTER_CHUNK_SIZE)
            val deferredChunks = ArrayList<Deferred<List<DatenFilm>>>()
            var startIndex = 0
            while (startIndex < filmSnapshot.size) {
                val endIndex = (startIndex + chunkSize).coerceAtMost(filmSnapshot.size)
                val chunkStartIndex = startIndex
                val chunkEndIndex = endIndex
                deferredChunks.add(
                    async(Dispatchers.Default) {
                        filterFilmRange(filmSnapshot, chunkStartIndex, chunkEndIndex, filterDuplicates, predicate)
                    }
                )
                startIndex = endIndex
            }

            val filteredChunks = deferredChunks.awaitAll()
            val filteredFilms = ArrayList<DatenFilm>()
            var chunkIndex = 0
            while (chunkIndex < filteredChunks.size) {
                filteredFilms.addAll(filteredChunks[chunkIndex])
                chunkIndex++
            }
            filteredFilms
        }
    }

    private fun filterFilmRange(
        filmSnapshot: List<DatenFilm>,
        startIndex: Int,
        endIndex: Int,
        filterDuplicates: Boolean,
        predicate: Predicate<DatenFilm>
    ): List<DatenFilm> {
        val filteredFilms = ArrayList<DatenFilm>(endIndex - startIndex)
        var index = startIndex
        while (index < endIndex) {
            val film = filmSnapshot[index]
            if (shouldKeepFilm(film, filterDuplicates, predicate)) {
                filteredFilms.add(film)
            }
            index++
        }
        return filteredFilms
    }

    private fun calculateZeitraumBoundaries() {
        try {
            val gui = MediathekGui.ui()
            val strZeitraum = gui?.tabFilme?.currentZeitraumFilterValue ?: ZeitraumSpinner.INFINITE_TEXT
            daysLowerBoundary =
                if (strZeitraum.equals(ZeitraumSpinner.INFINITE_TEXT, ignoreCase = true)) {
                    0
                } else {
                    val daysMs = strZeitraum.toLong().days.inWholeMilliseconds
                    System.currentTimeMillis() - daysMs
                }
        } catch (_: Exception) {
            daysLowerBoundary = 0
        }
    }

    private fun calculateMinimumFilmLength() {
        minimumFilmLength = ApplicationConfiguration.getInstance().blacklistMinimumFilmLengthMinutes.toLong() * 60
    }

    /**
     * Load current filter settings from Config.
     */
    private fun loadCurrentFilterSettings() {
        calculateZeitraumBoundaries()
        calculateMinimumFilmLength()

        val applicationConfiguration = ApplicationConfiguration.getInstance()
        blacklistIsActive = applicationConfiguration.isBlacklistEnabled
        doNotShowFutureFilms = applicationConfiguration.blacklistDoNotShowFutureFilms
        doNotShowGeoBlockedFilms = applicationConfiguration.blacklistDoNotShowGeoblockedFilms

        geoblockingPredicate.updateLocation()
    }

    /**
     * Check film based on date.
     */
    private fun checkDate(film: DatenFilm): Boolean {
        // always show livestreams
        if (film.isLivestream) {
            return true
        }

        if (daysLowerBoundary != 0L) {
            val filmTime = film.datumFilmTimeMillis
            return filmTime == 0L || filmTime >= daysLowerBoundary
        }

        return true
    }

    /**
     * Check if a future film should be displayed.
     */
    private fun checkIfFilmIsInFuture(film: DatenFilm): Boolean =
        film.datumFilmTimeMillis <= System.currentTimeMillis()

    /**
     * Filter based on film length.
     */
    private fun checkFilmLength(film: DatenFilm): Boolean =
        checkFilmLength(film, minimumFilmLength)

    class GeoblockingPredicate : Predicate<DatenFilm> {
        /**
         * Stores the current user's location. Can be modified by another thread.
         */
        private var geoLocation: Country = ApplicationConfiguration.getInstance().geographicLocation

        fun updateLocation() {
            geoLocation = ApplicationConfiguration.getInstance().geographicLocation
        }

        override fun test(film: DatenFilm): Boolean =
            !film.hasCountries() || film.hasCountry(geoLocation)
    }

    companion object {
        private const val COROUTINE_FILTER_THRESHOLD = 20_000
        private const val CHUNKS_PER_WORKER = 4
        private const val MIN_FILTER_CHUNK_SIZE = 1_000

        private fun shouldKeepFilm(
            film: DatenFilm,
            filterDuplicates: Boolean,
            predicate: Predicate<DatenFilm>
        ): Boolean =
            (!filterDuplicates || !film.isDuplicate) && predicate.test(film)

        private fun checkFilmLength(film: DatenFilm, minimumFilmLength: Long): Boolean {
            val filmLength = film.filmLength
            return !(filmLength != 0 && minimumFilmLength > filmLength)
        }
    }
}
