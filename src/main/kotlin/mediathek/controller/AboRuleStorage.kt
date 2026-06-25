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
package mediathek.controller

import kotlinx.serialization.KSerializer
import kotlinx.serialization.EncodeDefault
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonDecoder
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonEncoder
import kotlinx.serialization.json.JsonNull
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.decodeFromJsonElement
import kotlinx.serialization.json.encodeToJsonElement
import kotlinx.serialization.json.jsonPrimitive
import mediathek.daten.abo.DatenAbo
import mediathek.daten.abo.FilmLengthState
import mediathek.tool.FileUtils
import org.apache.logging.log4j.LogManager
import java.nio.file.Path
import java.time.LocalDate
import java.time.format.DateTimeParseException
import kotlin.io.path.createDirectories
import kotlin.io.path.deleteIfExists
import kotlin.io.path.exists
import kotlin.io.path.readText
import kotlin.io.path.writeText

object AboRuleStorage {
    private const val FILE_VERSION = 1
    private const val CLASSIC_RULE_TYPE = "classic"
    private val logger = LogManager.getLogger(AboRuleStorage::class.java)
    private val json = Json {
        ignoreUnknownKeys = true
        prettyPrint = true
        encodeDefaults = false
        explicitNulls = false
    }

    fun read(storagePath: Path): List<DatenAbo> {
        if (!storagePath.exists()) {
            return emptyList()
        }

        return json.decodeFromString<AboRulesFileDto>(storagePath.readText())
            .rules
            .mapNotNull(::ruleFromJson)
    }

    fun write(storagePath: Path, abos: Iterable<DatenAbo>) {
        storagePath.parent?.createDirectories()
        val temporaryPath = storagePath.resolveSibling(storagePath.fileName.toString() + ".tmp")
        try {
            val file = AboRulesFileDto(
                version = FILE_VERSION,
                rules = abos.map { abo ->
                    json.encodeToJsonElement(ClassicAboRuleDto.fromAbo(abo)) as JsonObject
                },
            )
            temporaryPath.writeText(json.encodeToString(file))
            FileUtils.moveAtomicallyWithFallback(temporaryPath, storagePath)
        } finally {
            temporaryPath.deleteIfExists()
        }
    }

    private fun ruleFromJson(rule: JsonObject): DatenAbo? {
        val type = rule["type"]?.jsonPrimitive?.content ?: CLASSIC_RULE_TYPE
        return when (type) {
            CLASSIC_RULE_TYPE -> json.decodeFromJsonElement<ClassicAboRuleDto>(rule).toAbo()
            else -> {
                logger.warn("Ignoring unsupported abo rule type: {}", type)
                null
            }
        }
    }
}

@Serializable
private data class AboRulesFileDto(
    @OptIn(ExperimentalSerializationApi::class)
    @EncodeDefault
    val version: Int = 1,
    val rules: List<JsonObject> = emptyList(),
)

@Serializable
private data class ClassicAboRuleDto(
    @OptIn(ExperimentalSerializationApi::class)
    @EncodeDefault
    val type: String = "classic",
    val active: Boolean = true,
    val name: String = "",
    val sender: String = "",
    val topic: String = "",
    val title: String = "",
    val topicTitle: String = "",
    val anywhere: String = "",
    val minimumDurationMinutes: Int = 0,
    val filmLengthMode: FilmLengthModeDto = FilmLengthModeDto.MINIMUM,
    val targetPath: String = "",
    @Serializable(with = NullableIsoLocalDateSerializer::class)
    val lastDownloadDate: LocalDate? = null,
    val programSet: String = "",
    val doNotStartAutomatically: Boolean = false,
) {
    fun toAbo(): DatenAbo =
        DatenAbo().apply {
            isActive = active
            name = this@ClassicAboRuleDto.name
            sender = this@ClassicAboRuleDto.sender
            thema = topic
            title = this@ClassicAboRuleDto.title
            themaTitel = topicTitle
            irgendwo = anywhere
            mindestDauerMinuten = minimumDurationMinutes
            filmLengthState = filmLengthMode.toDomain()
            zielpfad = targetPath
            downloadDate = lastDownloadDate
            psetName = programSet
            isDoNotStartAutomatically = doNotStartAutomatically
        }

    companion object {
        fun fromAbo(abo: DatenAbo): ClassicAboRuleDto =
            ClassicAboRuleDto(
                active = abo.isActive,
                name = abo.name,
                sender = abo.sender,
                topic = abo.thema,
                title = abo.title,
                topicTitle = abo.themaTitel,
                anywhere = abo.irgendwo,
                minimumDurationMinutes = abo.mindestDauerMinuten,
                filmLengthMode = FilmLengthModeDto.fromDomain(abo.filmLengthState),
                targetPath = abo.zielpfad,
                lastDownloadDate = abo.downloadDate,
                programSet = abo.psetName,
                doNotStartAutomatically = abo.isDoNotStartAutomatically,
            )
    }
}

@Serializable
private enum class FilmLengthModeDto {
    @SerialName("minimum")
    MINIMUM,

    @SerialName("maximum")
    MAXIMUM,
    ;

    fun toDomain(): FilmLengthState =
        when (this) {
            MINIMUM -> FilmLengthState.MINIMUM
            MAXIMUM -> FilmLengthState.MAXIMUM
        }

    companion object {
        fun fromDomain(value: FilmLengthState): FilmLengthModeDto =
            when (value) {
                FilmLengthState.MINIMUM -> MINIMUM
                FilmLengthState.MAXIMUM -> MAXIMUM
            }
    }
}

private object NullableIsoLocalDateSerializer : KSerializer<LocalDate?> {
    override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("NullableIsoLocalDate", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: LocalDate?) {
        val jsonEncoder = encoder as? JsonEncoder ?: error("NullableIsoLocalDateSerializer only supports JSON")
        jsonEncoder.encodeJsonElement(value?.let { JsonPrimitive(it.toString()) } ?: JsonNull)
    }

    override fun deserialize(decoder: Decoder): LocalDate? {
        val jsonDecoder = decoder as? JsonDecoder ?: error("NullableIsoLocalDateSerializer only supports JSON")
        return parse(jsonDecoder.decodeJsonElement())
    }

    private fun parse(element: JsonElement): LocalDate? {
        if (element is JsonNull) {
            return null
        }
        val value = (element as? JsonPrimitive)?.content?.takeIf { it.isNotBlank() } ?: return null
        return try {
            LocalDate.parse(value)
        } catch (ex: DateTimeParseException) {
            null
        }
    }
}
