package mediathek.tool

/**
 * Leftover only used in RuntimeExec class.
 */
object SysMsg {
    @JvmStatic
    fun playerMsg(text: String) {
        playermeldung(arrayOf(text))
    }

    private fun playermeldung(texte: Array<String>) {
        println("  >> ${texte[0]}")
        for (i in 1 until texte.size)
            println("  >> ${texte[i]}")
    }
}