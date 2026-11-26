package mediathek.controller.history

import mediathek.daten.DatenFilm
import java.util.BitSet

/** Matches history entries against catalog URLs and stores one seen bit per distinct URL. */
internal class SeenFilmUrls(films: Collection<DatenFilm>) {
    // A power-of-two table with at least half its slots empty keeps lookups short.
    private val urls = arrayOfNulls<String>(Math.multiplyExact(Integer.highestOneBit(films.size), 4).coerceAtLeast(4))
    private val mask = urls.lastIndex
    private val seen = BitSet(urls.size)

    init {
        for (film in films) {
            val url = film.urlNormalQuality
            if (url.isNotBlank()) {
                urls[slotFor(url)] = url
            }
        }
    }

    fun isSeen(url: String): Boolean = seen[slotFor(url)]

    fun markSeen(url: String) {
        val slot = slotFor(url)
        if (urls[slot] != null) {
            seen.set(slot)
        }
    }

    /** Returns false when JDBC must decode the original value to preserve its text semantics. */
    fun markSeenIfAscii(bytes: ByteArray): Boolean {
        var hash = 0
        for (byte in bytes) {
            if (byte < 0) {
                return false
            }
            hash = 31 * hash + byte
        }

        var slot = initialSlot(hash)
        while (true) {
            val url = urls[slot] ?: return true
            if (url.hashCode() == hash && url.matchesAscii(bytes)) {
                seen.set(slot)
                return true
            }
            slot = (slot + 1) and mask
        }
    }

    private fun slotFor(url: String): Int {
        var slot = initialSlot(url.hashCode())
        while (true) {
            val candidate = urls[slot]
            if (candidate == null || candidate == url) {
                return slot
            }
            slot = (slot + 1) and mask
        }
    }

    private fun initialSlot(hash: Int): Int = (hash xor (hash ushr 16)) and mask

    private fun String.matchesAscii(bytes: ByteArray): Boolean {
        if (length != bytes.size) {
            return false
        }
        for (index in indices) {
            if (this[index].code != bytes[index].toInt()) {
                return false
            }
        }
        return true
    }
}
