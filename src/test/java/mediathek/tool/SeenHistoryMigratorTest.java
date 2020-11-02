package mediathek.tool;


import mediathek.controller.history.SeenHistoryMigrator;
import mediathek.tool.sql.SqlDatabaseConfig;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.sqlite.SQLiteDataSource;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.SQLException;

import static org.junit.jupiter.api.Assertions.*;

public class SeenHistoryMigratorTest {
    Path dirPath;
    Path histTxtPath;
    Path dbPath;


    @BeforeEach
    void setUp() throws IOException {
        dirPath = Files.createTempDirectory("migrator-test");
        histTxtPath = Files.createFile(dirPath.resolve("history.txt"));
        //System.out.println("history text path: " + histTxtPath);

        dbPath = dirPath.resolve("history.db");

        Files.deleteIfExists(dbPath);

        createHistoryFile();
    }

    @AfterEach
    void tearDown() throws IOException {
        Files.deleteIfExists(dbPath);
        Files.deleteIfExists(histTxtPath);
        Files.deleteIfExists(dirPath);
    }

    private void createHistoryFile() throws IOException {
        var file = new File(histTxtPath.toAbsolutePath().toString());
        try (var filewriter = new FileWriter(file);
                var writer = new BufferedWriter(filewriter)) {
            String testHistory = """
                    02.11.2020 |#| Die Erben der Nacht       |#| 1. Der Funke erwacht                      |###|  https://pmdonlinekika-a.akamaihd.net/mp4dyn/4/FCMS-4247b13b-883c-4026-b228-a0ecee90a2db-31e0be270130_42.mp4
                    02.11.2020 |#| Die Erben der Nacht       |#| 2. Das Schulschiff                        |###|  https://pmdonlinekika-a.akamaihd.net/mp4dyn/c/FCMS-c3ded72c-f7c8-4ee4-bed5-e863f2e04154-31e0be270130_c3.mp4
                    02.11.2020 |#| rbb SPORT                 |#| Der rbb macht Fitness: Übungen an der C   |###|  https://rbbmediapmdp-a.akamaihd.net/content/c5/23/c523dcb6-89e4-4faf-a32e-615dab9c8906/c523dcb6-89e4-4faf-a32e-615dab9c8906_hd-1800k.mp4
                    02.11.2020 |#| rbb SPORT                 |#| Der rbb macht Fitness: Übungen an der C   |###|  https://rbbmediapmdp-a.akamaihd.net/content/c5/23/c523dcb6-89e4-4faf-a32e-615dab9c8906/c523dcb6-89e4-4faf-a32e-615dab9c8906_hd-1800k.mp4
                    02.11.2020 |#| Bon Courage               |#| Folge 28/39: Possesivpronomen, Besitzve   |###|  http://cdn-storage.br.de/MUJIuUOVBwQIbtC2uKJDM6OhuLnC_2rc5K1S/_AiS/5ygg_-bg/8c7ac9a2-23aa-4825-a843-95154096b5fb_C.mp4
                    02.11.2020 |#| Mascha und der Bär        |#| 14. Maschas tollkühne Schlittenfahrt      |###|  https://pmdgeokika-a.akamaihd.net/mp4dyn/f/FCMS-f0237051-ee6c-4ac4-88eb-e3079d6bc8ca-31e0be270130_f0.mp4
                    02.11.2020 |#| Mascha und der Bär        |#| 13. Verstecken spielen                    |###|  https://pmdgeokika-a.akamaihd.net/mp4dyn/6/FCMS-6301b482-865f-4848-8ce5-7dcfcdec8c21-31e0be270130_63.mp4
                    02.11.2020 |#| Ein Fall für die Erdmänn  |#| 19. Die letzte Seite                      |###|  https://pmdonlinekika-a.akamaihd.net/mp4dyn/4/FCMS-4942390a-314b-49e8-b07a-79144ecff97b-31e0be270130_49.mp4
                    """;
            writer.write(testHistory);
        }
    }

    void testDbContent() throws SQLException {
        final var dbPathStr = dbPath.toAbsolutePath().toString();
        SQLiteDataSource dataSource = new SQLiteDataSource(SqlDatabaseConfig.getConfig());
        dataSource.setUrl("jdbc:sqlite:" + dbPathStr);

        var testSql = "SELECT COUNT(url) AS total FROM seen_history WHERE url = 'https://pmdonlinekika-a.akamaihd.net/mp4dyn/4/FCMS-4247b13b-883c-4026-b228-a0ecee90a2db-31e0be270130_42.mp4'";
        try (var connection = dataSource.getConnection();
             var statement = connection.createStatement()) {
            var rs = statement.executeQuery(testSql);
            rs.next();
            var count = rs.getInt(1);
            //we must have one return
            assertEquals(count, 1);
            rs.close();

            testSql = "SELECT COUNT(*) as total FROM seen_history";
            rs = statement.executeQuery(testSql);
            rs.next();
            count = rs.getInt(1);
            // there must be 8 entries in db
            assertEquals(count, 8);
            rs.close();
        }
    }

    @Test
    void migrationTest() throws Exception {
        //db must not exist
        assertTrue(Files.notExists(dbPath));

        try (var migrator = new SeenHistoryMigrator(histTxtPath,dbPath)) {
            assertTrue(migrator.needsMigration());
            migrator.migrate();
        }

        //now db must exist, history.txt must have been deleted
        assertTrue(Files.exists(dbPath));
        assertFalse(Files.exists(histTxtPath));

        testDbContent();
    }
}
