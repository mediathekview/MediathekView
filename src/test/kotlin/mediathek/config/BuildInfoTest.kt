package mediathek.config

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.*

internal class BuildInfoTest {

    @Test
    fun readsGitMetadataFromProperties() {
        val properties = Properties().apply {
            setProperty("git.branch", "develop")
            setProperty("git.commit.id.abbrev", "abc1234")
        }

        val buildInfo = BuildInfo.fromProperties(properties)

        assertEquals("develop", buildInfo.branch())
        assertEquals("abc1234", buildInfo.commitId())
        assertTrue(buildInfo.hasGitMetadata())
        if (Konstanten.APPLICATION_TYPE == ApplicationType.NIGHTLY)
            assertEquals("develop @ abc1234", buildInfo.formatForDisplay())
        else
            assertEquals("abc1234", buildInfo.formatForDisplay())
    }

    @Test
    fun fallsBackToUnknownWhenMetadataIsMissing() {
        val buildInfo = BuildInfo.fromProperties(Properties())

        assertEquals("unknown", buildInfo.branch())
        assertEquals("unknown", buildInfo.commitId())
        assertFalse(buildInfo.hasGitMetadata())
    }
}
