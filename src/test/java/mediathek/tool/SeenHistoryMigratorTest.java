package mediathek.tool;


import org.junit.After;
import org.junit.Before;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class SeenHistoryMigratorTest {
    Path dirPath = Paths.get("/Users/christianfranzke/Desktop/test");
    Path histTxtPath = dirPath.resolve("history.txt");
    Path dbPath = dirPath.resolve("history.db");

    @After
    void tearDown() throws IOException {
        Files.deleteIfExists(dbPath);
        Files.deleteIfExists(histTxtPath);
        Files.deleteIfExists(dirPath);
    }

    private void createHistoryFile() {

    }

    @Before
    void setUp() throws IOException {
        Files.deleteIfExists(dbPath);
        Files.deleteIfExists(histTxtPath);
        Files.deleteIfExists(dirPath);

        createHistoryFile();
    }

    @Test
    void failTest() {
        assertTrue(Files.notExists(dbPath));

        try (var migrator = new SeenHistoryMigrator(histTxtPath,dbPath)) {
            assertFalse(migrator.needsMigration());

        }
    }
}
