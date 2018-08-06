package mSearch.daten;

import com.jidesoft.utils.SystemInfo;
import mediathek.config.Daten;
import mediathek.tool.GuiFunktionen;
import org.apache.commons.dbcp2.*;
import org.apache.commons.pool2.ObjectPool;
import org.apache.commons.pool2.impl.GenericObjectPool;

import javax.sql.DataSource;
import java.io.Closeable;
import java.io.File;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class PooledDatabaseConnection implements Closeable {
    private static PooledDatabaseConnection INSTANCE;
    private final DataSource dataSource;

    private final ExecutorService databaseExecutor;

    private PooledDatabaseConnection() {
        dataSource = setupDataSource();

        final int cpu = Runtime.getRuntime().availableProcessors();
        databaseExecutor = new ThreadPoolExecutor(cpu, 2 * cpu + 1, 15, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
        ((ThreadPoolExecutor) databaseExecutor).allowCoreThreadTimeOut(true);
    }

    public ExecutorService getDatabaseExecutor() {
        return databaseExecutor;
    }

    public static PooledDatabaseConnection getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new PooledDatabaseConnection();
        }
        return INSTANCE;
    }

    public void close() {
        connectionPool.close();
    }

    public Connection getConnection() {
        Connection con = null;
        try {
            con = dataSource.getConnection();
        } catch (SQLException e) {
            e.printStackTrace();
        } catch (IllegalStateException ignored) {
        }

        return con;
    }

    private ObjectPool<PoolableConnection> connectionPool;

    /**
     * Get the location of the filmlist database
     *
     * @return string to database location based on OS
     */
    private String getDatabaseLocation() {
        String strDatabase;

        if (SystemInfo.isMacOSX()) {
            //place database into OS X user cache directory in order not to backup it all the time in TimeMachine...
            strDatabase = GuiFunktionen.getHomePath() + File.separator + "Library/Caches/MediathekView/database" + File.separator;
        } else {
            strDatabase = Daten.getSettingsDirectory_String() + File.separator + "database" + File.separator;
        }

        return strDatabase;
    }

    private DataSource setupDataSource() {

        try {
            Class.forName("org.h2.Driver");
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }

        Properties props = new Properties();
        props.put("maxTotal", String.valueOf(Runtime.getRuntime().availableProcessors() * 2 + 1));
        props.put("poolPreparedStatements", "false");
        props.put("maxIdle", "-1");
        props.put("testOnBorrow", "true");

        final String driverCommand = "jdbc:h2:file:" + getDatabaseLocation() + "mediathekview;DB_CLOSE_DELAY=-1;DB_CLOSE_ON_EXIT=FALSE;AUTO_RECONNECT=TRUE";
        ConnectionFactory connectionFactory = new DriverManagerConnectionFactory(driverCommand, props);

        PoolableConnectionFactory poolableConnectionFactory =
                new PoolableConnectionFactory(connectionFactory, null);

        connectionPool = new GenericObjectPool<>(poolableConnectionFactory);

        poolableConnectionFactory.setPool(connectionPool);
        
        return new PoolingDataSource<>(connectionPool);
    }
}
