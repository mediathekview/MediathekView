package mediathek.daten.blacklist

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class ApplyBlacklistFilterPredicateTest {

    @Test
    fun mySplit() {
        val result = ApplyBlacklistFilterPredicate(ListeBlacklist()).mySplit("a,b,c,d")

        assertEquals("a", result[0])
        assertEquals("b", result[1])
        assertEquals("c", result[2])
        assertEquals("d", result[3])
    }
}
