package mediathek.daten

import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import mediathek.config.Config
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.tool.GuiFunktionen
import mediathek.tool.MemoryUtils
import org.apache.commons.lang3.SystemUtils
import java.io.File
import java.nio.file.Paths

object PooledDatabaseConnection {
    val dataSource: HikariDataSource?

    /**
     * Get the location of the filmlist database
     *
     * @return string to database location based on OS
     */
    @JvmStatic
    val databaseLocation: String
        get() {
            val strDatabase: String = if (Config.isPortableMode()) {
                Daten.getSettingsDirectory_String() + File.separator + "database" + File.separator
            } else {
                databaseCacheDirectory
            }
            val filePath = Paths.get(strDatabase)
            val absolutePath = filePath.toAbsolutePath()
            return absolutePath.toString()
        }

    @JvmStatic
    val databaseCacheDirectory: String
        get() {
            return if (SystemUtils.IS_OS_MAC_OSX) {
                //place database into OS X user cache directory in order not to backup it all the time in TimeMachine...
                GuiFunktionen.getHomePath() + File.separator + Konstanten.OSX_CACHE_DIRECTORY_NAME + File.separator
            } else {
                Daten.getSettingsDirectory_String() + File.separator
            }
        }

    init {
        if (MemoryUtils.isLowMemoryEnvironment()) {
            val driverCommand = "jdbc:h2:file:$databaseLocation/mediathekview;DB_CLOSE_DELAY=-1;DB_CLOSE_ON_EXIT=FALSE;AUTO_RECONNECT=TRUE"
            val config = HikariConfig()
            config.dataSourceClassName = "org.h2.jdbcx.JdbcDataSource"
            config.isAutoCommit = false
            config.addDataSourceProperty("URL", driverCommand)
            config.maximumPoolSize = Runtime.getRuntime().availableProcessors()
            dataSource = HikariDataSource(config)
        }
        else
            dataSource = null
    }
}