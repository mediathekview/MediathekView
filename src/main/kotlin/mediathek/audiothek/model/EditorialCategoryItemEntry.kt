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

package mediathek.audiothek.model

import java.net.URI
import java.time.OffsetDateTime

data class EditorialCategoryItemEntry(
    val title: String,
    val assetId: String,
    val pageUrl: URI?,
    val sender: EditorialCategoryPublicationService?,
    val isPublished: Boolean,
    val publishDate: OffsetDateTime?,
    val episodeNumber: Int?,
    val description: String?,
    val durationSeconds: Int?,
    val audioAssets: List<EditorialCategoryAudioAsset>,
) {
    fun preferredAudioAsset(): EditorialCategoryAudioAsset? {
        return preferredAudioAsset(audioAssets)
    }

    companion object {
        fun preferredAudioAsset(audioAssets: List<EditorialCategoryAudioAsset>): EditorialCategoryAudioAsset? {
            return audioAssets.maxWithOrNull(AUDIO_ASSET_PREFERENCE)
        }
        private val AUDIO_ASSET_PREFERENCE = compareBy<EditorialCategoryAudioAsset>(
            { it.downloadUrl != null },
            { it.mimeType.contains("mpeg", ignoreCase = true) },
            { qualityRank(it) },
            { hostRank(it) },
            { it.audioUrl != null },
            { it.downloadUrl?.toString().orEmpty() },
            { it.audioUrl?.toString().orEmpty() },
            { it.title },
        )

        private fun qualityRank(asset: EditorialCategoryAudioAsset): Int {
            val value = buildString {
                append(asset.title.lowercase())
                append(' ')
                append(asset.downloadUrl?.toString()?.lowercase().orEmpty())
                append(' ')
                append(asset.audioUrl?.toString()?.lowercase().orEmpty())
            }
            return when {
                ".s." in value || " stereo" in value -> 4
                ".m." in value || " mono" in value -> 3
                " standard" in value || " medium" in value -> 2
                " small" in value || " low" in value -> 1
                else -> 2
            }
        }

        private fun hostRank(asset: EditorialCategoryAudioAsset): Int {
            val value = asset.downloadUrl?.host ?: asset.audioUrl?.host ?: return 0
            return when {
                "akamaihd.net" in value -> 2
                else -> 1
            }
        }
    }
}
