package mediathek.sqlite

import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test
import org.sqlite.SQLiteErrorCode
import org.sqlite.SQLiteException
import java.nio.file.Path

class SeenHistoryCorruptionHandlerTest {
    @Test
    fun wrapsSqliteCorruptionException() {
        assertThrows(SeenHistoryCorruptionHandler.CorruptSeenHistoryDatabaseException::class.java) {
            SeenHistoryCorruptionHandler.openStoreOrThrowCorrupt(Path.of("history.db")) {
                throw SQLiteException("database disk image is malformed", SQLiteErrorCode.SQLITE_CORRUPT)
            }
        }
    }

    @Test
    fun doesNotWrapOperationalSqliteFailure() {
        assertThrows(SQLiteException::class.java) {
            SeenHistoryCorruptionHandler.openStoreOrThrowCorrupt(Path.of("history.db")) {
                throw SQLiteException("database is locked", SQLiteErrorCode.SQLITE_BUSY)
            }
        }
    }
}
