package mediathek.tool

import java.text.Collator
import java.util.*

object GermanStringSorter : Comparator<String> {
    private val collator: Collator = Collator.getInstance(Locale.GERMANY).apply {
        // ignore lower/upper case, but accept special characters in localised alphabetical order
        strength = Collator.SECONDARY
    }

    override fun compare(o1: String, o2: String): Int = collator.compare(o1, o2)
}
