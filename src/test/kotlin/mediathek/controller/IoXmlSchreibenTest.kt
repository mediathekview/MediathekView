package mediathek.controller

import mediathek.config.Daten
import mediathek.config.DatenXmlConfigDataFactory
import mediathek.controller.starter.DownloadRunState
import mediathek.controller.starter.StartStatus
import mediathek.daten.*
import mediathek.daten.abo.DatenAbo
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path

internal class IoXmlSchreibenTest {
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
    fun exportPsetWritesProgramSetAndProgramsInImportableFormat() {
        val pset = DatenPset("Save").apply {
            this[DatenPset.PROGRAMMSET_PRAEFIX_DIREKT] = "http"
            zielPfad = "/tmp/downloads"
            zielDateiname = "%t-%T"
            this[DatenPset.PROGRAMMSET_INFO_URL] = "https://example.invalid/info"
            setSpeichern(true)
            isThemaAnlegen = true
            aufloesung = FilmResolution.Enum.HIGH_QUALITY
            addProg(
                DatenProg(
                    "ffmpeg",
                    "/usr/bin/ffmpeg",
                    "-i %f **",
                    true.toString(),
                    false.toString(),
                ).apply {
                    targetFileName = "%t.mp4"
                    prefix = "http"
                    suffix = "m3u8"
                },
            )
        }
        val exportFile = tempDir.resolve("pset.xml")

        IoXmlSchreiben(DatenXmlConfigDataFactory.from(daten)).exportPset(arrayOf(pset), exportFile.toString())

        assertTrue(Files.exists(exportFile))
        val imported = ListePsetVorlagen.importPsetFile(exportFile.toString(), false)

        assertNotNull(imported)
        requireNotNull(imported)
        assertEquals(1, imported.size)
        val importedPset = imported[0]
        assertEquals("Save", importedPset.name)
        assertEquals("http", importedPset.getPraefixDirekt())
        assertEquals("/tmp/downloads", importedPset.zielPfad)
        assertEquals("%t-%T", importedPset.zielDateiname)
        assertEquals("https://example.invalid/info", importedPset[DatenPset.PROGRAMMSET_INFO_URL])
        assertTrue(importedPset.istSpeichern())
        assertEquals(FilmResolution.Enum.HIGH_QUALITY, importedPset.aufloesung)
        assertEquals(1, importedPset.listeProg.size)

        val importedProg = importedPset.getProg(0)
        assertEquals("ffmpeg", importedProg.name)
        assertEquals("/usr/bin/ffmpeg", importedProg.programPath)
        assertEquals("-i %f **", importedProg.switches)
        assertEquals("%t.mp4", importedProg.targetFileName)
        assertEquals("http", importedProg.prefix)
        assertEquals("m3u8", importedProg.suffix)
        assertTrue(importedProg.isRestart)
    }

    @Test
    fun writeConfigurationFileWritesDownloadsToJsonOnly() {
        daten.downloads.clearQueuedDownloads()
        daten.downloads.addLoadedDownloads(
            listOf(
                DatenDownload().apply {
                    title = "Queued Download"
                    downloadUrl = "https://example.invalid/download.mp4"
                    targetPathFileName = "/tmp/download.mp4"
                    art = DownloadType.DIRECT
                    quelle = DownloadSource.DOWNLOAD
                    init()
                },
                DatenDownload().apply {
                    title = "Finished Download"
                    art = DownloadType.DIRECT
                    quelle = DownloadSource.DOWNLOAD
                    runtime.runState = DownloadRunState().also { it.status = StartStatus.FINISHED }
                    init()
                },
            ),
        )
        val configFile = tempDir.resolve("mediathek.xml")
        val storageFile = tempDir.resolve("downloads.json")

        IoXmlSchreiben(DatenXmlConfigDataFactory.from(daten), downloadStoragePath = storageFile)
            .writeConfigurationFile(configFile)

        val xml = Files.readString(configFile)
        assertTrue(Files.exists(storageFile))
        assertFalse(xml.contains("<Downlad>"))
        assertEquals(listOf("Queued Download"), DownloadStorage.read(storageFile).map(DatenDownload::title))
    }

    @Test
    fun writeConfigurationFileDoesNotWriteAbosToXml() {
        val abos = daten.abos.list
        val originalAbos = ArrayList(abos)
        try {
            abos.clear()
            abos.add(
                DatenAbo().apply {
                    name = "Legacy Writer Abo"
                    sender = "ARD"
                    title = "tagesschau"
                },
            )
            val configFile = tempDir.resolve("mediathek.xml")

            IoXmlSchreiben(
                DatenXmlConfigDataFactory.from(daten),
                downloadStoragePath = tempDir.resolve("downloads.json"),
            ).writeConfigurationFile(configFile)

            val xml = Files.readString(configFile)
            assertFalse(xml.contains("<Abonnement>"))
            assertFalse(xml.contains("Legacy Writer Abo"))
        } finally {
            abos.clear()
            abos.addAll(originalAbos)
        }
    }
}
