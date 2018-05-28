package mSearch.daten;

import com.jidesoft.utils.SystemInfo;
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
import java.util.concurrent.Executors;

public class PooledDatabaseConnection implements Closeable {
    private static PooledDatabaseConnection INSTANCE;
    private final DataSource dataSource;

    private final ExecutorService databaseExecutor;

    private PooledDatabaseConnection() {
        dataSource = setupDataSource();

        databaseExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
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
        }
        return con;
    }

    private ObjectPool<PoolableConnection> connectionPool;

    private DataSource setupDataSource() {
        final String CACHE_PATH;
        if (SystemInfo.isMacOSX()) {
            CACHE_PATH = System.getProperty("user.home") + "/Library/Caches/MediathekView/database/";
        } else
            CACHE_PATH = System.getProperty("user.home") + File.separatorChar + ".mediathek3" + File.separatorChar
                    + "database" + File.separatorChar;

        Properties props = new Properties();
        //props.put("defaultAutoCommit","false");
        props.put("maxTotal", String.valueOf(Runtime.getRuntime().availableProcessors()));
        props.put("poolPreparedStatements", "true");
        final String driverCommand = "jdbc:h2:file:" + CACHE_PATH + "mediathekview;MVCC=TRUE";
        ConnectionFactory connectionFactory = new DriverManagerConnectionFactory(driverCommand, props);

        PoolableConnectionFactory poolableConnectionFactory =
                new PoolableConnectionFactory(connectionFactory, null);

        connectionPool = new GenericObjectPool<>(poolableConnectionFactory);

        poolableConnectionFactory.setPool(connectionPool);

        return new PoolingDataSource<>(connectionPool);
    }
}
