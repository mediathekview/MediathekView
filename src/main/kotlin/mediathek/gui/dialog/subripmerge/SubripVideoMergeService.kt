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

package mediathek.gui.dialog.subripmerge

import com.github.kokorin.jaffree.ffmpeg.FFmpeg
import com.github.kokorin.jaffree.ffmpeg.FFmpegResult
import com.github.kokorin.jaffree.ffmpeg.UrlInput
import com.github.kokorin.jaffree.ffmpeg.UrlOutput
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import mediathek.tool.GuiFunktionenProgramme
import java.nio.file.Path

object SubripVideoMergeService {
    suspend fun merge(
        subripFilePath: String,
        videoFilePath: String,
        videoOutputPath: String,
        languageCode: String,
    ): FFmpegResult = withContext(Dispatchers.IO) {
        val ffmpegPath: Path = GuiFunktionenProgramme.findExecutableOnPath("ffmpeg").parent
        FFmpeg.atPath(ffmpegPath)
            .setOverwriteOutput(true)
            .addArgument("-xerror")
            .addInput(UrlInput.fromUrl(videoFilePath))
            .addInput(UrlInput.fromUrl(subripFilePath))
            .addOutput(UrlOutput.toUrl(videoOutputPath))
            .addArguments("-c", "copy")
            .addArguments("-c:s", "mov_text")
            .addArgument("-metadata:s:s:0")
            .addArgument("language=$languageCode")
            .execute()
    }
}
