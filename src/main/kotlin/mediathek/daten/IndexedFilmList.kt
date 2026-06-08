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

package mediathek.daten

import mediathek.config.StandardLocations
import mediathek.tool.ApplicationConfiguration
import org.apache.logging.log4j.LogManager
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.store.Directory
import java.nio.file.Path
import kotlin.math.max

class IndexedFilmList : ListeFilme() {
    var luceneDirectory: Directory? = null
        private set

    var reader: DirectoryReader? = null

    private var filmNrIndex: Map<Int, DatenFilm>? = null

    init {
        try {
            luceneDirectory = createLuceneDirectory(StandardLocations.getFilmIndexPath())
        } catch (ex: Exception) {
            logger.error("Creation of Lucene index directory failed!", ex)
        }
    }

    @Throws(Exception::class)
    private fun createLuceneDirectory(indexPath: Path): Directory {
        val configuredMode = ApplicationConfiguration.getConfiguration()
            .getString(ApplicationConfiguration.LUCENE_DIRECTORY_MODE, "auto")
        val mode = LuceneDirectoryMode.fromConfigValueOrNull(configuredMode)
            ?: run {
                logger.warn("Unknown Lucene directory mode '{}', falling back to 'auto'", configuredMode)
                LuceneDirectoryMode.AUTO
            }

        logger.info("Using Lucene directory mode '{}' for index path {}", mode.configValue, indexPath)
        return mode.createDirectory(indexPath)
    }

    @Synchronized
    fun getFilmByFilmNr(filmNr: Int): DatenFilm? {
        val index = filmNrIndex
        if (index == null || index.size != size) {
            rebuildFilmNrIndex()
        }
        return filmNrIndex?.get(filmNr)
    }

    private fun rebuildFilmNrIndex() {
        val rebuiltIndex = HashMap<Int, DatenFilm>(max(16, size))
        for (film in this) {
            rebuiltIndex[film.filmNr] = film
        }
        filmNrIndex = rebuiltIndex
    }

    @Synchronized
    override fun clear() {
        super.clear()
        filmNrIndex = null
    }

    @Synchronized
    override fun add(element: DatenFilm): Boolean {
        filmNrIndex = null
        return super.add(element)
    }

    @Synchronized
    override fun add(index: Int, element: DatenFilm) {
        filmNrIndex = null
        super.add(index, element)
    }

    @Synchronized
    override fun addAll(elements: Collection<DatenFilm>): Boolean {
        filmNrIndex = null
        return super.addAll(elements)
    }

    @Synchronized
    override fun addAll(index: Int, elements: Collection<DatenFilm>): Boolean {
        filmNrIndex = null
        return super.addAll(index, elements)
    }

    @Synchronized
    override fun removeAt(index: Int): DatenFilm {
        filmNrIndex = null
        return super.removeAt(index)
    }

    @Synchronized
    override fun remove(element: DatenFilm): Boolean {
        val index = indexOf(element)
        if (index < 0) {
            return false
        }
        removeAt(index)
        return true
    }

    @Synchronized
    override fun set(index: Int, element: DatenFilm): DatenFilm {
        filmNrIndex = null
        return super.set(index, element)
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}
