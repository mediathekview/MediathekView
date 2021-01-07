package mediathek.tool

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

internal class VersionTest {

    @Test
    fun isOlderThan() {
        val v1 = Version(13,6,0)
        val v2 = Version(13,7,0)

        assertTrue { v1.isOlderThan(v2) }
        assertFalse { v2.isOlderThan(v1) }
    }

    @Test
    fun isInvalid() {
        val invVersion = Version.INVALID_VERSION
        assertTrue {invVersion.isInvalid() }

        val manInv = Version(0,0,0)
        assertTrue { manInv.isInvalid() }

        val t3 = Version(13,6,0)
        assertFalse { t3.isInvalid() }
    }

    @Test
    fun fromString() {
        val str = "13.7.0"
        val v = Version.fromString(str)
        assertTrue { v == Version(13,7,0) }

        val t2 = "13.6.0-SNAPSHOT"
        val v2 = Version.fromString(t2)
        assertTrue { v2 == Version(13,6,0) }
    }
}