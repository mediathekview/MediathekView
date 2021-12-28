package mediathek.daten.blacklist

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Test
import java.io.StringWriter
import javax.xml.stream.XMLOutputFactory
import kotlin.test.assertTrue

internal class BlacklistRuleTest {
    @Test
    fun test_distinct_with_duplicates() {
        val list = ListeBlacklist()

        val rule1 = BlacklistRule("ZDF")
        val rule2 = BlacklistRule("ZDF")
        list.addWithoutNotification(rule1)
        list.addWithoutNotification(rule2)

        assertTrue { list.size == 2 }

        // remove duplicates
        val distinctList = list.stream().distinct().toList()
        assertTrue { distinctList.size == 1 }
    }

    @Test
    fun test_distinct_no_duplicates() {
        val list = ListeBlacklist()

        val rule1 = BlacklistRule("ZDF")
        val rule2 = BlacklistRule("ARD")
        list.addWithoutNotification(rule1)
        list.addWithoutNotification(rule2)

        assertTrue { list.size == 2 }

        // try to remove duplicates
        val distinctList = list.stream().distinct().toList()

        // rule objects are not equal therefore we must have 2 if comparator works
        assertTrue { distinctList.size == 2 }
    }

    @Test
    fun test_comparator() {
        val rule1 = BlacklistRule("ARD", "thema1", "titel1", "thema_titel1")
        val rule2 = BlacklistRule("ARD1", "thema1", "titel1", "thema_titel1")
        // copy of rule1
        val rule3 = BlacklistRule("ARD", "thema1", "titel1", "thema_titel1")

        assertFalse { rule1 == rule2 }
        assertFalse { rule2 == rule3 }
        assertTrue { rule1 == rule3 }
    }

    @Test
    fun test_non_empty_config_writer() {
        val stringWriter = StringWriter()
        var outStr: String
        stringWriter.use {
            val xmlWriter = XMLOutputFactory.newInstance().createXMLStreamWriter(it)
            val rule1 = BlacklistRule("ARD", "", "titel1", "thema_titel1")
            rule1.writeToConfig(xmlWriter)
            outStr = stringWriter.toString()

            xmlWriter.close()
        }

        //thema must not exist in xml
        assertFalse { outStr.contains("<black-thema>") }
        assertTrue { outStr.contains("<black-titel>") }
        assertTrue { outStr.contains("<black-thema-titel>") }
        assertTrue { outStr.contains("<black-sender>") }
    }
}