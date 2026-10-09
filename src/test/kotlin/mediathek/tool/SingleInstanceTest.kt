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

package mediathek.tool

import mediathek.config.StandardLocations
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Path

class SingleInstanceTest {
    @TempDir
    lateinit var settingsDirectory: Path

    @AfterEach
    fun tearDown() {
        StandardLocations.portableBaseDirectory = null
    }

    @Test
    fun instance1_not_active() {
        StandardLocations.portableBaseDirectory = settingsDirectory.toString()

        SingleInstance().use { instance1 ->
            assertFalse(instance1.isAppAlreadyActive())
        }
    }

    @Test
    fun instance2_activity_test() {
        StandardLocations.portableBaseDirectory = settingsDirectory.toString()

        SingleInstance().use { instance1 ->
            SingleInstance().use { instance2 ->
                assertFalse(instance1.isAppAlreadyActive())
                assertTrue(instance2.isAppAlreadyActive())
            }
        }
    }
}
