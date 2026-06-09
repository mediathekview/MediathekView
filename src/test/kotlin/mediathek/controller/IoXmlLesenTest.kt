package mediathek.controller

import mediathek.config.Daten
import mediathek.daten.*
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path

internal class IoXmlLesenTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun datenLesenReadsProgramSetsAndFollowingPrograms() {
        val listePset = Daten.getInstance().listePset
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

            assertTrue(IoXmlLesen().datenLesen(configFile))

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
        assertFalse(IoXmlLesen().datenLesen(tempDir.resolve("missing.xml")))
    }

    @Test
    fun datenLesenMigratesLegacyDownloadsToJson() {
        val downloads = Daten.getInstance().listeDownloads
        val originalDownloads = ArrayList(downloads)
        try {
            downloads.clear()
            val configFile = tempDir.resolve("mediathek.xml")
            val storageFile = tempDir.resolve("downloads.json")
            Files.writeString(
                configFile,
                """
                <?xml version="1.0" encoding="UTF-8"?>
                <Mediathek>
                    <Downlad>
                        <Nr>1</Nr>
                        <Sender>ARD</Sender>
                        <Thema>Legacy Topic</Thema>
                        <Titel>Legacy Download</Titel>
                        <URL>https://example.invalid/legacy.mp4</URL>
                        <Art>1</Art>
                        <Quelle>2</Quelle>
                    </Downlad>
                </Mediathek>
                """.trimIndent(),
            )

            assertTrue(IoXmlLesen(storageFile).datenLesen(configFile))

            assertTrue(Files.exists(storageFile))
            assertEquals(1, downloads.size)
            assertEquals("Legacy Download", downloads.single().title)
            assertEquals("Legacy Download", DownloadStorage.read(storageFile).single().title)
        } finally {
            downloads.clear()
            downloads.addAll(originalDownloads)
        }
    }

    @Test
    fun datenLesenUsesJsonDownloadsWhenPresent() {
        val downloads = Daten.getInstance().listeDownloads
        val originalDownloads = ArrayList(downloads)
        try {
            downloads.clear()
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

            assertTrue(IoXmlLesen(storageFile).datenLesen(configFile))

            assertEquals(1, downloads.size)
            assertEquals("JSON Download", downloads.single().title)
        } finally {
            downloads.clear()
            downloads.addAll(originalDownloads)
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
