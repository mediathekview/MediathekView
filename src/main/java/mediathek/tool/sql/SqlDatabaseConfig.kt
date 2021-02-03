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
            conf.setSharedCache(true)
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