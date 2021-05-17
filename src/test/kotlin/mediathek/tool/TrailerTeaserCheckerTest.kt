package mediathek.tool

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

internal class TrailerTeaserCheckerTest {

    @Test
    fun successTest1() {
        val checker = TrailerTeaserChecker()

        val testStr = "TrAiLeR"
        assertTrue {checker.check(testStr)}
    }

    @Test
    fun successTest2() {
        val checker = TrailerTeaserChecker()

        val testStr = "TeAsEr"
        assertTrue {checker.check(testStr)}
    }

    @Test
    fun successTest3() {
        val checker = TrailerTeaserChecker()

        val testStr = "VoRsChAu"
        assertTrue {checker.check(testStr)}

    }

    @Test
    fun failTest1() {
        val checker = TrailerTeaserChecker()

        val testStr = "bla"
        assertFalse {checker.check(testStr)}
    }

    @Test
    fun failTest2() {
        val checker = TrailerTeaserChecker()

        val testStr = "BlA"
        assertFalse {checker.check(testStr)}
    }

}