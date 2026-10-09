package mediathek.gui.tabs.tab_online_search

import kotlinx.serialization.json.*

internal fun String.parseJsonObject(json: Json): JsonObject = json.parseToJsonElement(this).jsonObject

internal fun JsonObject.string(name: String): String? = this[name]?.jsonPrimitive?.contentOrNull

internal fun JsonElement.jsonObjectOrNull(): JsonObject? = this as? JsonObject

internal fun JsonElement.jsonArrayOrNull(): JsonArray? = this as? JsonArray
