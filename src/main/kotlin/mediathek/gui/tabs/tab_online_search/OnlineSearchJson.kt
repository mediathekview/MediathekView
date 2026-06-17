package mediathek.gui.tabs.tab_online_search

import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonArray
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive

internal fun String.parseJsonObject(json: Json): JsonObject = json.parseToJsonElement(this).jsonObject

internal fun JsonObject.string(name: String): String? = this[name]?.jsonPrimitive?.contentOrNull

internal fun JsonElement.jsonObjectOrNull(): JsonObject? = this as? JsonObject

internal fun JsonElement.jsonArrayOrNull(): JsonArray? = this as? JsonArray
