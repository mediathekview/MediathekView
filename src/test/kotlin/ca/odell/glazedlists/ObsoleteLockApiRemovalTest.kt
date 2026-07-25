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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Test
import java.nio.file.Files
import java.nio.file.Path

internal class ObsoleteLockApiRemovalTest {
    @Test
    fun obsoleteLockHelperSourcesAreRemoved() {
        listOf(
            "src/main/java/ca/odell/glazedlists/Guard.java",
            "src/main/kotlin/ca/odell/glazedlists/impl/ThreadSafeList.kt",
            "src/main/java/ca/odell/glazedlists/LockbasedSyncListener.java",
        ).forEach { sourcePath ->
            assertFalse(Files.exists(Path.of(sourcePath)), sourcePath)
        }
    }

    @Test
    fun obsoleteLockMethodsAreNotPartOfTheApi() {
        val eventListMethods = EventList::class.java.declaredMethods.map { it.name }.toSet()
        assertFalse(eventListMethods.any { it.startsWith("acceptWith") || it.startsWith("applyWith") })

        val glazedListsMethods = GlazedLists::class.java.declaredMethods.map { it.name }.toSet()
        assertFalse("threadSafeList" in glazedListsMethods)
        assertFalse("syncEventListToEventList" in glazedListsMethods)
    }
}
