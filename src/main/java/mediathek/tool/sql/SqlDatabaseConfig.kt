/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.tool.sql

import mediathek.config.StandardLocations
import org.sqlite.SQLiteConfig
import org.sqlite.SQLiteDataSource
import java.nio.file.Path

object SqlDatabaseConfig {
    @JvmStatic
    val dataSource: SQLiteDataSource

    @JvmStatic
    val historyDbPath: Path = StandardLocations.getSettingsDirectory().resolve("history.db")

    @JvmStatic
    val config: SQLiteConfig
        get() {
            val conf = SQLiteConfig()
            conf.setEncoding(SQLiteConfig.Encoding.UTF8)
            conf.setLockingMode(SQLiteConfig.LockingMode.NORMAL)
            conf.setSharedCache(false)
            conf.setSynchronous(SQLiteConfig.SynchronousMode.OFF)
            conf.enableLoadExtension(false)
            conf.setJournalMode(SQLiteConfig.JournalMode.WAL)
            conf.setPageSize(4096)
            return conf
        }

    init {
        dataSource = SQLiteDataSource(config)
        dataSource.url = "jdbc:sqlite:" + historyDbPath.toAbsolutePath().toString()
    }
}