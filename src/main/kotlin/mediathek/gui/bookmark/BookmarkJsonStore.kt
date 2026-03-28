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

package mediathek.gui.bookmark

import ca.odell.glazedlists.EventList
import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.*
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption
import java.time.LocalDate

object BookmarkJsonStore {
    private val json = Json {
        prettyPrint = true
        explicitNulls = false
        encodeDefaults = true
        ignoreUnknownKeys = true
    }

    @JvmStatic
    fun read(path: Path): MutableList<BookmarkData> {
        val content = Files.readString(path)
        val wrapper = json.decodeFromString(BookmarksWrapper.serializer(), content)
        return wrapper.bookmarks.map { it.toBookmarkData() }.toMutableList()
    }

    @JvmStatic
    fun write(path: Path, bookmarks: EventList<BookmarkData>) {
        val wrapper = BookmarksWrapper(bookmarks = bookmarks.map { BookmarkDto.fromBookmarkData(it) })
        val content = json.encodeToString(BookmarksWrapper.serializer(), wrapper)
        val parent = path.parent
        if (parent != null) {
            Files.createDirectories(parent)
        }
        Files.writeString(
            path,
            content,
            StandardOpenOption.CREATE,
            StandardOpenOption.TRUNCATE_EXISTING,
            StandardOpenOption.WRITE
        )
    }
}

@Serializable
private data class BookmarksWrapper(
    @SerialName("bookmarks")
    val bookmarks: List<BookmarkDto> = emptyList()
)

@Serializable
private data class BookmarkDto(
    @SerialName("seen")
    val seen: Boolean = false,
    @SerialName("url")
    val url: String? = null,
    @SerialName("note")
    val note: String? = null,
    @SerialName("availableUntil")
    @Serializable(with = LegacyLocalDateSerializer::class)
    val availableUntil: LocalDate? = null,
    @SerialName("bookmarkAdded")
    @Serializable(with = LegacyLocalDateSerializer::class)
    val bookmarkAdded: LocalDate? = null,
    @SerialName("filmHashCode")
    val filmHashCode: String? = null,
    @SerialName("originalSender")
    val originalSender: String? = null,
    @SerialName("originalTitle")
    val originalTitle: String? = null,
    @SerialName("originalThema")
    val originalThema: String? = null
) {
    fun toBookmarkData(): BookmarkData {
        val result = BookmarkData()
        result.setSeen(seen)
        result.setUrl(url)
        result.setNote(note)
        result.setAvailableUntil(availableUntil)
        result.setBookmarkAdded(bookmarkAdded)
        result.originalSender = originalSender
        result.originalTitle = originalTitle
        result.originalThema = originalThema
        if (filmHashCode != null) {
            result.setFilmHashCode(filmHashCode)
        }
        return result
    }

    companion object {
        fun fromBookmarkData(value: BookmarkData): BookmarkDto {
            return BookmarkDto(
                seen = value.seen,
                url = value.url,
                note = value.note,
                availableUntil = value.availableUntil,
                bookmarkAdded = value.bookmarkAdded,
                filmHashCode = value.filmHashCode,
                originalSender = value.originalSender,
                originalTitle = value.originalTitle,
                originalThema = value.originalThema
            )
        }
    }
}

private object LegacyLocalDateSerializer : KSerializer<LocalDate> {
    override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("LegacyLocalDate", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: LocalDate) {
        val jsonEncoder = encoder as? JsonEncoder ?: error("LegacyLocalDateSerializer only supports JSON")
        jsonEncoder.encodeJsonElement(
            buildJsonArray {
                add(JsonPrimitive(value.year))
                add(JsonPrimitive(value.monthValue))
                add(JsonPrimitive(value.dayOfMonth))
            }
        )
    }

    override fun deserialize(decoder: Decoder): LocalDate {
        val jsonDecoder = decoder as? JsonDecoder ?: error("LegacyLocalDateSerializer only supports JSON")
        return when (val element = jsonDecoder.decodeJsonElement()) {
            is JsonArray -> parseArrayDate(element)
            is JsonPrimitive -> LocalDate.parse(element.content)
            else -> throw IllegalArgumentException("Unsupported LocalDate JSON: $element")
        }
    }

    private fun parseArrayDate(array: JsonArray): LocalDate {
        if (array.size < 3) {
            throw IllegalArgumentException("LocalDate array needs at least 3 elements: $array")
        }
        val year = array[0].jsonPrimitive.int
        val month = array[1].jsonPrimitive.int
        val day = array[2].jsonPrimitive.int
        return LocalDate.of(year, month, day)
    }

    private val JsonElement.jsonPrimitive: JsonPrimitive
        get() = this as? JsonPrimitive ?: throw IllegalArgumentException("Expected primitive JSON value: $this")

    private val JsonPrimitive.int: Int
        get() = this.content.toIntOrNull() ?: throw IllegalArgumentException("Expected int JSON value: $this")
}
