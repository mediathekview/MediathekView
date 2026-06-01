package mediathek.daten

class FilmResolution private constructor() {
    enum class Enum(private val legacyText: String) {
        LOW(FilmResolution.LOW),
        NORMAL(FilmResolution.NORMAL),
        HIGH_QUALITY(FilmResolution.HIGH_QUALITY),
        ;

        override fun toString(): String = legacyText

        companion object {
            @JvmStatic
            fun fromLegacyString(input: String): Enum =
                when (input) {
                    FilmResolution.LOW -> LOW
                    FilmResolution.HIGH_QUALITY -> HIGH_QUALITY
                    else -> NORMAL
                }
        }
    }

    companion object {
        const val NORMAL: String = "normal"
        const val HIGH_QUALITY: String = "hd"
        const val LOW: String = "klein"
    }
}
