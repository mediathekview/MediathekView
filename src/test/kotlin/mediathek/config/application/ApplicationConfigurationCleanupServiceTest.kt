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

package mediathek.config.application

import org.apache.commons.configuration2.XMLConfiguration
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Path
import java.util.*

class ApplicationConfigurationCleanupServiceTest {
    @Test
    fun `annotated registry accepts exact and dynamic keys`() {
        val filterId = UUID.randomUUID()

        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.dark_mode"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("config.major"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("search.history.items_lucene"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("download.toolbar.state.main.x"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("filter.available.filters.filter_$filterId"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("filter.filter_$filterId.show.new_only"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("ui.bookmark-dialog.colummn-settings"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("abo-v3.sortKeys"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("online-search.colummn-settings"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("film.sortKeys"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("film.colummn-settings"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.ui.online_search.show"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.ui.online_search.ard.search.history"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.ui.online_search.ard.url.history"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.ui.online_search.zdf.search.history"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.ui.online_search.zdf.url.history"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.ui.online_search.arte.search.history"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.ui.online_search.arte.url.history"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.ui.online_search.arte.request.delay.millis"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.ui.download_table.column_configuration"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.ui.download_table.sender_icons.show"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.ui.download_table.sender_icons.small"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.ui.film_table.column_configuration"))
        assertTrue(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("application.ui.blacklist_table.column_configuration"))

        assertFalse(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("filter.filter_not-a-uuid.show.new_only"))
        assertFalse(AnnotatedApplicationConfigurationKeyRegistry.isValidKey("obsolete.application.key"))
    }

    @Test
    fun `cleanup removes keys not known by annotated registry`(@TempDir tempDir: Path) {
        val filterId = UUID.randomUUID()
        val config = createConfiguration(filterId)

        val statistics = ApplicationConfigurationCleanupService(config).cleanup(
            settingsPath = tempDir.resolve("settings.xml"),
            backupPath = null,
            dryRun = false,
        )

        assertEquals(5, statistics.totalKeysBefore)
        assertEquals(3, statistics.totalKeysAfter)
        assertFalse(statistics.dryRun)
        assertEquals(
            listOf("filter.filter_not-a-uuid.thema", "obsolete.application.key"),
            statistics.removedKeys,
        )
        assertTrue(config.containsKey("application.dark_mode"))
        assertTrue(config.containsKey("download.toolbar.state.main.orientation"))
        assertTrue(config.containsKey("filter.filter_$filterId.thema"))
        assertFalse(config.containsKey("obsolete.application.key"))
        assertFalse(config.containsKey("filter.filter_not-a-uuid.thema"))
    }

    @Test
    fun `dry run reports removable keys without changing configuration`(@TempDir tempDir: Path) {
        val filterId = UUID.randomUUID()
        val config = createConfiguration(filterId)

        val statistics = ApplicationConfigurationCleanupService(config).cleanup(
            settingsPath = tempDir.resolve("settings.xml"),
            backupPath = null,
            dryRun = true,
        )

        assertEquals(5, statistics.totalKeysBefore)
        assertEquals(5, statistics.totalKeysAfter)
        assertTrue(statistics.dryRun)
        assertEquals(
            listOf("filter.filter_not-a-uuid.thema", "obsolete.application.key"),
            statistics.removedKeys,
        )
        assertTrue(config.containsKey("application.dark_mode"))
        assertTrue(config.containsKey("download.toolbar.state.main.orientation"))
        assertTrue(config.containsKey("filter.filter_$filterId.thema"))
        assertTrue(config.containsKey("obsolete.application.key"))
        assertTrue(config.containsKey("filter.filter_not-a-uuid.thema"))
    }

    private fun createConfiguration(filterId: UUID): XMLConfiguration = XMLConfiguration().apply {
        addProperty("application.dark_mode", true)
        addProperty("download.toolbar.state.main.orientation", 1)
        addProperty("filter.filter_$filterId.thema", "Test")
        addProperty("obsolete.application.key", "remove")
        addProperty("filter.filter_not-a-uuid.thema", "remove")
    }
}
