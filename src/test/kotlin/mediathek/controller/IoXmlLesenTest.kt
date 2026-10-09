package mediathek.controller

import mediathek.config.Daten
import mediathek.config.DatenXmlConfigDataFactory
import mediathek.daten.*
import mediathek.daten.abo.DatenAbo
import mediathek.daten.abo.FilmLengthState
import mediathek.daten.blacklist.BlacklistRule
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path
import java.time.LocalDate

internal class IoXmlLesenTest {
    @TempDir
    lateinit var tempDir: Path
    private lateinit var daten: Daten

    @BeforeEach
    fun setUp() {
        daten = Daten()
    }

    @AfterEach
    fun tearDown() {
        daten.downloads.shutdown()
    }

    @Test
    fun datenLesenReadsProgramSetsAndFollowingPrograms() {
        val listePset = daten.programSets.list
        val originalState = ListePset()
        originalState.addAll(listePset)
        try {
            listePset.clear()
            val configFile = tempDir.resolve("mediathek.xml")
            Files.writeString(
                configFile,
                """
                <?xml version="1.0" encoding="UTF-8"?>
                <Mediathek>
                    <Programmset>
                        <Name>Save</Name>
                        <Zielpfad>/tmp/downloads</Zielpfad>
                        <Speichern>true</Speichern>
                        <Info-URL>https://example.invalid/info</Info-URL>
                    </Programmset>
                    <Programm>
                        <Programmname>ffmpeg</Programmname>
                        <Zieldateiname>%t.mp4</Zieldateiname>
                        <Programmpfad>/usr/bin/ffmpeg</Programmpfad>
                        <Programmschalter>-i %f **</Programmschalter>
                        <Praefix>http</Praefix>
                        <Suffix>m3u8</Suffix>
                        <Restart>true</Restart>
                    </Programm>
                    <Programmset>
                        <Name>Play</Name>
                        <Abspielen>true</Abspielen>
                    </Programmset>
                    <Programm>
                        <Programmname>vlc</Programmname>
                        <Programmpfad>/usr/bin/vlc</Programmpfad>
                    </Programm>
                </Mediathek>
                """.trimIndent(),
            )

            assertTrue(
                IoXmlLesen(
                    DatenXmlConfigDataFactory.from(daten),
                    downloadStoragePath = tempDir.resolve("downloads.json"),
                    blacklistRuleStoragePath = tempDir.resolve("blacklist-rules.json"),
                ).datenLesen(configFile),
            )

            assertEquals(2, listePset.size)
            val save = listePset[0]
            assertEquals("Save", save.name)
            assertEquals("/tmp/downloads", save.zielPfad)
            assertTrue(save.istSpeichern())
            assertEquals("https://example.invalid/info", save[DatenPset.PROGRAMMSET_INFO_URL])
            assertEquals(1, save.listeProg.size)
            assertProgram(
                save.getProg(0),
                name = "ffmpeg",
                targetFileName = "%t.mp4",
                programPath = "/usr/bin/ffmpeg",
                switches = "-i %f **",
            )
            assertEquals("http", save.getProg(0).prefix)
            assertEquals("m3u8", save.getProg(0).suffix)
            assertTrue(save.getProg(0).isRestart)

            val play = listePset[1]
            assertEquals("Play", play.name)
            assertTrue(play.istAbspielen())
            assertEquals(1, play.listeProg.size)
            assertEquals("vlc", play.getProg(0).name)
        } finally {
            listePset.clear()
            listePset.addAll(originalState)
        }
    }

    @Test
    fun datenLesenReturnsFalseWhenFileDoesNotExist() {
        assertFalse(IoXmlLesen(DatenXmlConfigDataFactory.from(daten)).datenLesen(tempDir.resolve("missing.xml")))
    }

    @Test
    fun datenLesenMigratesLegacyDownloadsToJson() {
        daten.downloads.clearQueuedDownloads()
        val configFile = tempDir.resolve("mediathek.xml")
        val storageFile = tempDir.resolve("downloads.json")
        Files.writeString(
            configFile,
            """
                <?xml version="1.0" encoding="UTF-8"?>
                <Mediathek>
                    <Downlad>
                        <Nr>42</Nr>
                        <Sender>ARD</Sender>
                        <Thema>Legacy Topic</Thema>
                        <Titel>Legacy Download</Titel>
                        <URL>https://example.invalid/legacy.mp4</URL>
                        <Art>1</Art>
                        <Quelle>2</Quelle>
                    </Downlad>
                    <Downlad>
                        <Nr>99</Nr>
                        <Sender>ZDF</Sender>
                        <Thema>Legacy Topic</Thema>
                        <Titel>Second Legacy Download</Titel>
                        <URL>https://example.invalid/second-legacy.mp4</URL>
                        <Art>1</Art>
                        <Quelle>2</Quelle>
                    </Downlad>
                </Mediathek>
                """.trimIndent(),
        )

        assertTrue(
            IoXmlLesen(
                DatenXmlConfigDataFactory.from(daten),
                downloadStoragePath = storageFile,
                blacklistRuleStoragePath = tempDir.resolve("blacklist-rules.json"),
            ).datenLesen(configFile),
        )

        val downloads = daten.downloads.queuedDownloads()
        assertTrue(Files.exists(storageFile))
        assertEquals(2, downloads.size)
        assertEquals("Legacy Download", downloads[0].title)
        assertEquals(1, downloads[0].nr)
        assertEquals("Second Legacy Download", downloads[1].title)
        assertEquals(2, downloads[1].nr)
        assertEquals(
            listOf("Legacy Download", "Second Legacy Download"),
            DownloadStorage.read(storageFile).map(DatenDownload::title),
        )
    }

    @Test
    fun datenLesenUsesJsonDownloadsWhenPresent() {
        daten.downloads.clearQueuedDownloads()
        val configFile = tempDir.resolve("mediathek.xml")
        val storageFile = tempDir.resolve("downloads.json")
        Files.writeString(
            configFile,
            """
                <?xml version="1.0" encoding="UTF-8"?>
                <Mediathek>
                    <Downlad>
                        <Titel>Legacy Download</Titel>
                        <Art>1</Art>
                        <Quelle>2</Quelle>
                    </Downlad>
                </Mediathek>
                """.trimIndent(),
        )
        DownloadStorage.write(
            storageFile,
            listOf(
                DatenDownload().apply {
                    title = "JSON Download"
                    art = DownloadType.DIRECT
                    quelle = DownloadSource.DOWNLOAD
                    init()
                },
            ),
        )

        assertTrue(
            IoXmlLesen(
                DatenXmlConfigDataFactory.from(daten),
                downloadStoragePath = storageFile,
                blacklistRuleStoragePath = tempDir.resolve("blacklist-rules.json"),
            ).datenLesen(configFile),
        )

        val downloads = daten.downloads.queuedDownloads()
        assertEquals(1, downloads.size)
        assertEquals("JSON Download", downloads.single().title)
    }

    @Test
    fun datenLesenMigratesLegacyBlacklistRulesToJson() {
        val blacklist = daten.blacklist.rules
        val originalBlacklist = ArrayList(blacklist)
        try {
            blacklist.clear()
            val configFile = tempDir.resolve("mediathek.xml")
            val downloadStorageFile = tempDir.resolve("downloads.json")
            val blacklistStorageFile = tempDir.resolve("blacklist-rules.json")
            Files.writeString(
                configFile,
                """
                <?xml version="1.0" encoding="UTF-8"?>
                <Mediathek>
                    <Blacklist>
                        <black-sender>ARD</black-sender>
                        <black-thema>News</black-thema>
                        <black-titel>tagesschau</black-titel>
                        <black-thema-titel>News tagesschau</black-thema-titel>
                    </Blacklist>
                </Mediathek>
                """.trimIndent(),
            )

            assertTrue(
                IoXmlLesen(
                    DatenXmlConfigDataFactory.from(daten),
                    downloadStoragePath = downloadStorageFile,
                    blacklistRuleStoragePath = blacklistStorageFile,
                ).datenLesen(configFile),
            )

            assertTrue(Files.exists(blacklistStorageFile))
            assertEquals(listOf(BlacklistRule("ARD", "News", "tagesschau", "News tagesschau")), blacklist)
            assertEquals(
                listOf(BlacklistRule("ARD", "News", "tagesschau", "News tagesschau")),
                BlacklistRuleStorage.read(blacklistStorageFile),
            )
        } finally {
            blacklist.clear()
            blacklist.addAll(originalBlacklist)
        }
    }

    @Test
    fun datenLesenUsesJsonBlacklistRulesWhenPresent() {
        val blacklist = daten.blacklist.rules
        val originalBlacklist = ArrayList(blacklist)
        try {
            blacklist.clear()
            val configFile = tempDir.resolve("mediathek.xml")
            val downloadStorageFile = tempDir.resolve("downloads.json")
            val blacklistStorageFile = tempDir.resolve("blacklist-rules.json")
            Files.writeString(
                configFile,
                """
                <?xml version="1.0" encoding="UTF-8"?>
                <Mediathek>
                    <Blacklist>
                        <black-sender>Legacy</black-sender>
                    </Blacklist>
                </Mediathek>
                """.trimIndent(),
            )
            BlacklistRuleStorage.write(blacklistStorageFile, listOf(BlacklistRule(sender = "JSON")))

            assertTrue(
                IoXmlLesen(
                    DatenXmlConfigDataFactory.from(daten),
                    downloadStoragePath = downloadStorageFile,
                    blacklistRuleStoragePath = blacklistStorageFile,
                ).datenLesen(configFile),
            )

            assertEquals(listOf(BlacklistRule(sender = "JSON")), blacklist)
        } finally {
            blacklist.clear()
            blacklist.addAll(originalBlacklist)
        }
    }

    @Test
    fun datenLesenMigratesLegacyAbosToJson() {
        val abos = daten.abos.list
        val originalAbos = ArrayList(abos)
        try {
            abos.clear()
            val configFile = tempDir.resolve("mediathek.xml")
            val aboRulesFile = tempDir.resolve("abo-rules.json")
            Files.writeString(
                configFile,
                """
                <?xml version="1.0" encoding="UTF-8"?>
                <Mediathek>
                    <Abonnement>
                        <aktiv>false</aktiv>
                        <Name>Legacy Abo</Name>
                        <Sender>ARD</Sender>
                        <Thema>News</Thema>
                        <Titel>tagesschau</Titel>
                        <Thema-Titel>Politics</Thema-Titel>
                        <Irgendwo>Berlin</Irgendwo>
                        <Mindestdauer>15</Mindestdauer>
                        <min_max>false</min_max>
                        <Zielpfad>/tmp/legacy</Zielpfad>
                        <letztes_Abo>25.06.2026</letztes_Abo>
                        <Programmset>Save</Programmset>
                        <nicht_automatisch_starten>true</nicht_automatisch_starten>
                    </Abonnement>
                </Mediathek>
                """.trimIndent(),
            )

            assertTrue(
                IoXmlLesen(
                    DatenXmlConfigDataFactory.from(daten),
                    downloadStoragePath = tempDir.resolve("downloads.json"),
                    blacklistRuleStoragePath = tempDir.resolve("blacklist-rules.json"),
                    aboRuleStoragePath = aboRulesFile,
                ).datenLesen(configFile),
            )

            assertTrue(Files.exists(aboRulesFile))
            val loaded = abos.single()
            assertEquals("Legacy Abo", loaded.name)
            assertEquals("ARD", loaded.sender)
            assertEquals("News", loaded.thema)
            assertEquals("tagesschau", loaded.title)
            assertEquals("Politics", loaded.themaTitel)
            assertEquals("Berlin", loaded.irgendwo)
            assertEquals(15, loaded.mindestDauerMinuten)
            assertEquals(FilmLengthState.MAXIMUM, loaded.filmLengthState)
            assertEquals("/tmp/legacy", loaded.zielpfad)
            assertEquals(LocalDate.of(2026, 6, 25), loaded.downloadDate)
            assertEquals("Save", loaded.psetName)
            assertTrue(loaded.isDoNotStartAutomatically)
            assertEquals("Legacy Abo", AboRuleStorage.read(aboRulesFile).single().name)
        } finally {
            abos.clear()
            abos.addAll(originalAbos)
        }
    }

    @Test
    fun datenLesenUsesJsonAbosWhenPresent() {
        val abos = daten.abos.list
        val originalAbos = ArrayList(abos)
        try {
            abos.clear()
            val configFile = tempDir.resolve("mediathek.xml")
            val aboRulesFile = tempDir.resolve("abo-rules.json")
            Files.writeString(
                configFile,
                """
                <?xml version="1.0" encoding="UTF-8"?>
                <Mediathek>
                    <Abonnement>
                        <Name>Legacy Abo</Name>
                        <Sender>ARD</Sender>
                    </Abonnement>
                </Mediathek>
                """.trimIndent(),
            )
            AboRuleStorage.write(
                aboRulesFile,
                listOf(
                    DatenAbo().apply {
                        name = "JSON Abo"
                        sender = "ZDF"
                    },
                ),
            )

            assertTrue(
                IoXmlLesen(
                    DatenXmlConfigDataFactory.from(daten),
                    downloadStoragePath = tempDir.resolve("downloads.json"),
                    blacklistRuleStoragePath = tempDir.resolve("blacklist-rules.json"),
                    aboRuleStoragePath = aboRulesFile,
                ).datenLesen(configFile),
            )

            assertEquals(1, abos.size)
            assertEquals("JSON Abo", abos.single().name)
            assertEquals("ZDF", abos.single().sender)
        } finally {
            abos.clear()
            abos.addAll(originalAbos)
        }
    }

    private fun assertProgram(
        program: DatenProg,
        name: String,
        targetFileName: String,
        programPath: String,
        switches: String,
    ) {
        assertEquals(name, program.name)
        assertEquals(targetFileName, program.targetFileName)
        assertEquals(programPath, program.programPath)
        assertEquals(switches, program.switches)
    }
}
