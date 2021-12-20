package mediathek.daten.blacklist

import java.util.*

enum class BlacklistTags(val index: Int, val xmlName: String) {
    NR(0, "black-nr"),
    SENDER(1, "black-sender"),
    THEMA(2, "black-thema"),
    TITEL(3, "black-titel"),
    THEMA_TITEL(4, "black-thema-titel");

    companion object {
        @JvmStatic
        fun fromXmlTag(tag: String): Optional<BlacklistTags> {
            return Arrays.stream(values()).filter { e: BlacklistTags -> e.xmlName == tag }.findAny()
        }

        @JvmStatic
        fun fromIndex(index: Int): Optional<BlacklistTags> {
            return Arrays.stream(values()).filter { e: BlacklistTags -> e.index == index }.findAny()
        }
    }
}