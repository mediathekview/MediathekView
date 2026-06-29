package mediathek.gui.actions.export

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.daten.ListeFilme
import mediathek.filmlisten.writer.FilmListWriter
import java.io.File
import kotlin.math.roundToInt

data class FilmlistExportSettings(
    val compressSender: Boolean,
    val compressThema: Boolean
)

class FilmlistExportWorker(
    private val films: ListeFilme,
    private val selectedFile: File,
    private val exportSettings: FilmlistExportSettings,
    private val uiScope: CoroutineScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing),
    private val onProgress: (Int) -> Unit = {},
    private val onCompletion: (Boolean) -> Unit = {}
) {
    fun execute(): Job = uiScope.launch {
        val success = runCatching {
            withContext(Dispatchers.IO) {
                exportFilmlist()
            }
        }.isSuccess
        onCompletion(success)
    }

    private fun exportFilmlist() {
        val writer = FilmListWriter(true).apply {
            compressSenderTag = exportSettings.compressSender
            compressThemaTag = exportSettings.compressThema
            decompressUrls = true
        }
        writer.writeFilmList(
            selectedFile.absolutePath,
            films
        ) { prog ->
            uiScope.launch {
                onProgress((100.0 * prog).roundToInt())
            }
        }
    }
}
