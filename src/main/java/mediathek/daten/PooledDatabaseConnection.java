package mediathek.daten;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import mediathek.config.Config;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.tool.GuiFunktionen;
import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

public class PooledDatabaseConnection {
    private static PooledDatabaseConnection INSTANCE;
    private final HikariDataSource dataSource;

    private PooledDatabaseConnection() {
        dataSource = setupDataSource();
    }

    public static PooledDatabaseConnection getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new PooledDatabaseConnection();
        }
        return INSTANCE;
    }

    public HikariDataSource getDataSource() {
        return dataSource;
    }

    /**
     * Get the location of the filmlist database
     *
     * @return string to database location based on OS
     */
    public static String getDatabaseLocation() {
        String strDatabase;

        if (Config.isPortableMode()) {
            strDatabase = Daten.getSettingsDirectory_String() + File.separator + "database" + File.separator;
        } else {
            if (SystemUtils.IS_OS_MAC_OSX) {
                //place database into OS X user cache directory in order not to backup it all the time in TimeMachine...
                strDatabase = GuiFunktionen.getHomePath() + File.separator + Konstanten.OSX_CACHE_DIRECTORY_NAME + File.separator;
            } else {
                strDatabase = Daten.getSettingsDirectory_String() + File.separator + "database" + File.separator;
            }
        }

        final Path filePath = Paths.get(strDatabase);
        final Path absolutePath = filePath.toAbsolutePath();

        return absolutePath.toString();
    }

    private HikariDataSource setupDataSource() {
        final String driverCommand = "jdbc:h2:file:" + getDatabaseLocation() + "/mediathekview;DB_CLOSE_DELAY=-1;DB_CLOSE_ON_EXIT=FALSE;AUTO_RECONNECT=TRUE";

        HikariConfig config = new HikariConfig();
        config.setDataSourceClassName("org.h2.jdbcx.JdbcDataSource");
        config.addDataSourceProperty("URL", driverCommand);
        config.setMaximumPoolSize(Runtime.getRuntime().availableProcessors());

        return new HikariDataSource(config);
    }
}
