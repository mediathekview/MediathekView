package mediathek.tool

import org.apache.logging.log4j.LogManager

data class Version(val major: Int, val minor: Int, val patch: Int) {
    companion object {
        val INVALID_VERSION = Version(0, 0, 0)
        private val logger = LogManager.getLogger()

        @JvmStatic
        fun fromString(versionsstring: String): Version {
            val versions = versionsstring.replace("-SNAPSHOT", "").split(".", ignoreCase = true)
            return if (versions.size == 3) {
                try {
                    Version(
                        Integer.parseInt(versions[0]),
                        Integer.parseInt(versions[1]),
                        Integer.parseInt(versions[2])
                    )
                } catch (ex: Exception) {
                    logger.error("Fehler beim Parsen der Version: {}", versionsstring, ex)
                    INVALID_VERSION
                }
            } else
                INVALID_VERSION
        }
    }

    override fun toString(): String {
        return String.format("%d.%d.%d", major, minor, patch)
    }

    /**
     * Gibt die Version als gewichtete Zahl zurück.
     *
     * @return gewichtete Zahl als Integer
     */
    private fun toNumber(): Int {
        return major * 100 + minor * 10 + patch
    }

    /**
     * Check if other version is newer than we are.
     * @param other the other version to check.
     * @return true if other is newer, otherwise false.
     */
    fun isOlderThan(other: Version): Boolean {
        return other.toNumber() > this.toNumber()
    }

    /**
     * Check if this version is invalid.
     * @return true if invalid, false otherwise.
     */
    fun isInvalid(): Boolean {
        return this == INVALID_VERSION
    }
}