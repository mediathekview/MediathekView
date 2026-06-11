package mediathek.controller

import mediathek.config.Daten
import mediathek.controller.starter.DownloadRunState
import mediathek.controller.starter.StartStatus
import mediathek.daten.*
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path

internal class IoXmlSchreibenTest {
    @TempDir
    lateinit var tempDir: Path

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

        IoXmlSchreiben().exportPset(arrayOf(pset), exportFile.toString())

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
        val downloads = Daten.getInstance().listeDownloads
        val originalDownloads = ArrayList(downloads)
        try {
            downloads.clear()
            downloads.add(
                DatenDownload().apply {
                    title = "Queued Download"
                    downloadUrl = "https://example.invalid/download.mp4"
                    targetPathFileName = "/tmp/download.mp4"
                    art = DownloadType.DIRECT
                    quelle = DownloadSource.DOWNLOAD
                    init()
                },
            )
            downloads.add(
                DatenDownload().apply {
                    title = "Finished Download"
                    art = DownloadType.DIRECT
                    quelle = DownloadSource.DOWNLOAD
                    runtime.runState = DownloadRunState().also { it.status = StartStatus.FINISHED }
                    init()
                },
            )
            val configFile = tempDir.resolve("mediathek.xml")
            val storageFile = tempDir.resolve("downloads.json")

            IoXmlSchreiben(downloadStoragePath = storageFile).writeConfigurationFile(configFile)

            val xml = Files.readString(configFile)
            assertTrue(Files.exists(storageFile))
            assertFalse(xml.contains("<Downlad>"))
            assertEquals(listOf("Queued Download"), DownloadStorage.read(storageFile).map(DatenDownload::title))
        } finally {
            downloads.clear()
            downloads.addAll(originalDownloads)
        }
    }
}
