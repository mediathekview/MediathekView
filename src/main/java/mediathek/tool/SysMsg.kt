package mediathek.tool

import javafx.collections.FXCollections

object SysMsg {
    private val outputList = FXCollections.observableArrayList<String>()
    private const val MAX_STELLEN = 5
    private const val FUELL_ZEICHEN = "0"
    private var zeilenNrProgramm = 0

    @Synchronized
    @JvmStatic
    fun playerMsg(text: String) {
        playermeldung(arrayOf(text))
    }

    private fun playermeldung(texte: Array<String>) {
        val z = "  >>"
        println(z + " " + texte[0])
        notify(texte[0])
        for (i in 1 until texte.size) {
            println(z + " " + texte[i])
            notify(texte[i])
        }
    }

    private fun notify(zeile: String) {
        addText("[" + getNr(zeilenNrProgramm++) + "]   " + zeile)
    }

    private fun getNr(nr: Int): String {
        var str = nr.toString()
        while (str.length < MAX_STELLEN) {
            str = FUELL_ZEICHEN + str
        }
        return str
    }

    @Synchronized
    private fun addText(texte: String) {
        if (outputList.size > 50000) {
            outputList.remove(0, 30000)
        }
        outputList.add(texte + System.lineSeparator())
    }
}