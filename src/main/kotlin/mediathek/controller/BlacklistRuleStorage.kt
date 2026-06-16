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

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import mediathek.daten.blacklist.BlacklistRule
import mediathek.tool.FileUtils
import java.nio.file.Path
import kotlin.io.path.createDirectories
import kotlin.io.path.deleteIfExists
import kotlin.io.path.exists
import kotlin.io.path.readText
import kotlin.io.path.writeText

object BlacklistRuleStorage {
    private const val FILE_VERSION = 1
    private val json = Json {
        ignoreUnknownKeys = true
        prettyPrint = true
        encodeDefaults = false
    }

    fun read(storagePath: Path): List<BlacklistRule> {
        if (!storagePath.exists()) {
            return emptyList()
        }

        return json.decodeFromString<BlacklistRulesFileDto>(storagePath.readText())
            .rules
            .map(BlacklistRuleDto::toRule)
    }

    fun write(storagePath: Path, rules: Iterable<BlacklistRule>) {
        storagePath.parent?.createDirectories()
        val temporaryPath = storagePath.resolveSibling(storagePath.fileName.toString() + ".tmp")
        try {
            val file = BlacklistRulesFileDto(
                version = FILE_VERSION,
                rules = rules.distinctBy(BlacklistRule::criteria).map(BlacklistRuleDto::fromRule),
            )
            temporaryPath.writeText(json.encodeToString(file))
            FileUtils.moveAtomicallyWithFallback(temporaryPath, storagePath)
        } finally {
            temporaryPath.deleteIfExists()
        }
    }
}

@Serializable
private data class BlacklistRulesFileDto(
    val version: Int,
    val rules: List<BlacklistRuleDto>,
)

@Serializable
private data class BlacklistRuleDto(
    val sender: String = "",
    val topic: String = "",
    val title: String = "",
    val topicTitle: String = "",
    val active: Boolean = true,
) {
    fun toRule(): BlacklistRule =
        BlacklistRule(
            sender = sender,
            thema = topic,
            titel = title,
            thema_titel = topicTitle,
            active = active,
        )

    companion object {
        fun fromRule(rule: BlacklistRule): BlacklistRuleDto =
            BlacklistRuleDto(
                sender = rule.sender,
                topic = rule.thema,
                title = rule.titel,
                topicTitle = rule.thema_titel,
                active = rule.active,
            )
    }
}
