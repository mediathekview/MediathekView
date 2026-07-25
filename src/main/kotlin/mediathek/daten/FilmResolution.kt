package mediathek.daten

object FilmResolution {
    const val NORMAL: String = "normal"
    const val HIGH_QUALITY: String = "hd"
    const val LOW: String = "klein"

    enum class Enum(private val legacyText: String) {
        LOW(FilmResolution.LOW),
        NORMAL(FilmResolution.NORMAL),
        HIGH_QUALITY(FilmResolution.HIGH_QUALITY),
        ;

        override fun toString(): String = legacyText

        companion object {
            fun fromLegacyString(input: String): Enum =
                when (input) {
                    FilmResolution.LOW -> LOW
                    FilmResolution.HIGH_QUALITY -> HIGH_QUALITY
                    else -> NORMAL
                }
        }
    }
}
