/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.controller.starter

import com.github.kokorin.jaffree.ffmpeg.FFmpeg
import com.github.kokorin.jaffree.ffmpeg.UrlInput
import com.github.kokorin.jaffree.ffmpeg.UrlOutput
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenDownload
import mediathek.daten.DatenProg
import mediathek.tool.GuiFunktionenProgramme
import org.apache.logging.log4j.LogManager
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption
import java.time.LocalDate
import java.time.format.DateTimeFormatter

internal typealias Mp4MetadataRemuxer = (Path, Path, Path, Map<String, String>) -> Boolean

internal object Mp4Metadata {
    private val logger = LogManager.getLogger(Mp4Metadata::class.java)
    private val germanDateFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
    private val standardTagNames = setOf("title", "artist", "album", "date", "description", "synopsis", "comment")

    fun defaultFor(download: DatenDownload): Map<String, String> {
        val film = download.film
        val description = film?.description.orEmpty()
        return buildMap {
            putIfNotBlank("title", download.title.ifBlank { film?.title.orEmpty() })
            putIfNotBlank("artist", download.sender.ifBlank { film?.sender.orEmpty() })
            putIfNotBlank("album", download.topic.ifBlank { film?.thema.orEmpty() })
            putIfNotBlank("date", toIsoDate(download.date.ifBlank { film?.sendeDatum.orEmpty() }))
            putIfNotBlank("description", description)
            putIfNotBlank("synopsis", description)
            putIfNotBlank("comment", description)
        }.filterKeys { it in standardTagNames }
    }

    fun writeDefaultTags(
        download: DatenDownload,
        ffmpegExecutable: Path,
        remux: Mp4MetadataRemuxer = ::remuxWithJaffree,
    ): Boolean {
        if (!download.isMp4Metadata) return false
        val target = Paths.get(download.targetPathFileName)
        if (!target.fileName.toString().endsWith(".mp4", ignoreCase = true)) return false
        if (!Files.isRegularFile(target)) return false

        val metadata = defaultFor(download)
        if (metadata.isEmpty()) return false

        val temporaryTarget = target.resolveSibling(".${target.fileName}.metadata.tmp.mp4")
        return try {
            Files.deleteIfExists(temporaryTarget)
            if (remux(ffmpegExecutable, target, temporaryTarget, metadata)) {
                Files.move(temporaryTarget, target, StandardCopyOption.REPLACE_EXISTING)
                true
            } else {
                false
            }
        } catch (ex: Exception) {
            logger.warn("Fehler beim Schreiben der MP4-Metadaten: {}", target, ex)
            false
        } finally {
            runCatching { Files.deleteIfExists(temporaryTarget) }
        }
    }

    private fun remuxWithJaffree(
        ffmpegExecutable: Path,
        source: Path,
        target: Path,
        metadata: Map<String, String>,
    ): Boolean {
        FFmpeg.atPath(ffmpegExecutable.parent)
            .setOverwriteOutput(true)
            .addArgument("-xerror")
            .addInput(UrlInput.fromPath(source))
            .addOutput(remuxOutput(target, metadata))
            .execute()
        return true
    }

    internal fun remuxOutput(target: Path, metadata: Map<String, String>): UrlOutput {
        var output = UrlOutput.toPath(target)
            .addMap(0)
            .copyAllCodecs()

        metadata.forEach { (key, value) ->
            output = output.addArguments("-metadata", "$key=$value")
        }

        return output
    }

    private fun MutableMap<String, String>.putIfNotBlank(key: String, value: String) {
        if (value.isNotBlank()) {
            put(key, value)
        }
    }

    private fun toIsoDate(value: String): String =
        runCatching { LocalDate.parse(value, germanDateFormatter).toString() }
            .getOrDefault(value)
}

internal object FfmpegExecutableResolver {
    fun resolve(download: DatenDownload): Path? =
        resolve(
            download = download,
            configuredPath = ApplicationConfiguration.getInstance().standardFFmpegPath,
            pathLookup = { GuiFunktionenProgramme.findExecutableOnPath("ffmpeg") },
        )

    internal fun resolve(
        download: DatenDownload,
        configuredPath: String,
        pathLookup: () -> Path?,
    ): Path? =
        executableFromDownload(download)
            ?: runCatching { pathLookup() }.getOrNull()?.takeIf(::isExecutable)
            ?: executablePath(configuredPath)

    private fun executableFromDownload(download: DatenDownload): Path? {
        val programSet = download.pSet ?: return null
        val program = programSet.listeProg.firstOrNull { it.name == download.programName }
            ?: programSet.listeProg.firstOrNull { it.urlTesten(download.downloadUrl) }
            ?: return null
        return executableFromProgram(program)
    }

    private fun executableFromProgram(program: DatenProg): Path? = executablePath(program.programPath)

    private fun executablePath(path: String): Path? =
        path.takeIf(String::isNotBlank)
            ?.let(Paths::get)
            ?.takeIf(::isExecutable)

    private fun isExecutable(path: Path): Boolean = Files.isRegularFile(path) && Files.isExecutable(path)
}
