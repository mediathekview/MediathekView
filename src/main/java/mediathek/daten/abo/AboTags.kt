package mediathek.daten.abo

import java.util.*

enum class AboTags(val index: Int, val xmlName: String) {
    NR(DatenAbo.ABO_NR, "Nr"),
    EINGESCHALTET(DatenAbo.ABO_EINGESCHALTET, "aktiv"),
    NAME(DatenAbo.ABO_NAME, "Name"),
    SENDER(DatenAbo.ABO_SENDER, "Sender"),
    THEMA(DatenAbo.ABO_THEMA, "Thema"),
    TITEL(DatenAbo.ABO_TITEL, "Titel"),
    THEMA_TITEL(DatenAbo.ABO_THEMA_TITEL, "Thema-Titel"),
    IRGENDWO(DatenAbo.ABO_IRGENDWO, "Irgendwo"),
    MINDESTDAUER(DatenAbo.ABO_MINDESTDAUER, "Mindestdauer"),
    MIN(DatenAbo.ABO_MIN, "min_max"),
    ZIELPFAD(DatenAbo.ABO_ZIELPFAD, "Zielpfad"),
    DOWN_DATUM(DatenAbo.ABO_DOWN_DATUM, "letztes_Abo"),
    PSET(DatenAbo.ABO_PSET, "Programmset");

    companion object {
        @JvmStatic
        fun fromXmlTag(tag: String): Optional<AboTags> {
            return Arrays.stream(values()).filter { e: AboTags -> e.xmlName == tag }.findAny()
        }

        @JvmStatic
        fun fromIndex(index: Int): Optional<AboTags> {
            return Arrays.stream(values()).filter { e: AboTags -> e.index == index }.findAny()
        }
    }
}