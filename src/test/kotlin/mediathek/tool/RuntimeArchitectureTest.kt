package mediathek.tool

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class RuntimeArchitectureTest {
    @Test
    fun isIntelOrAmd64Bit_acceptsIntelAndAmd64Aliases() {
        assertTrue(RuntimeArchitecture.isIntelOrAmd64Bit("amd64"))
        assertTrue(RuntimeArchitecture.isIntelOrAmd64Bit("x86_64"))
        assertTrue(RuntimeArchitecture.isIntelOrAmd64Bit("AMD64"))
        assertTrue(RuntimeArchitecture.isIntelOrAmd64Bit("X86_64"))
    }

    @Test
    fun isIntelOrAmd64Bit_rejectsOtherArchitectures() {
        assertFalse(RuntimeArchitecture.isIntelOrAmd64Bit("aarch64"))
        assertFalse(RuntimeArchitecture.isIntelOrAmd64Bit("arm64"))
        assertFalse(RuntimeArchitecture.isIntelOrAmd64Bit("x86"))
    }
}
