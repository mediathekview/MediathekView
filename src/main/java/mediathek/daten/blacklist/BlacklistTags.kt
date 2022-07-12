package mediathek.daten.blacklist

import java.util.*

enum class BlacklistTags(val index: Int, val xmlName: String) {
    SENDER(0, "black-sender"),
    THEMA(1, "black-thema"),
    TITEL(2, "black-titel"),
    THEMA_TITEL(3, "black-thema-titel");

    companion object {
        @JvmStatic
        fun fromXmlTag(tag: String): Optional<BlacklistTags> {
            return Arrays.stream(values()).filter { e: BlacklistTags -> e.xmlName == tag }.findAny()
        }
    }
}