package mediathek.tool.sql;

import org.sqlite.SQLiteConfig;

public class SqlDatabaseConfig {
    public static SQLiteConfig getConfig() {
        var conf = new SQLiteConfig();
        conf.setEncoding(SQLiteConfig.Encoding.UTF8);
        conf.setLockingMode(SQLiteConfig.LockingMode.NORMAL);
        conf.setSharedCache(true);
        conf.setSynchronous(SQLiteConfig.SynchronousMode.OFF);
        conf.enableLoadExtension(false);
        return conf;
    }
}
