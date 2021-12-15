package mediathek.javafx.filterpanel

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import mediathek.tool.GermanStringSorter

object SenderListBoxModel {
    @JvmStatic
    val providedSenderList: EventList<String> = BasicEventList()

    init {
        providedSenderList.add("3Sat")
        providedSenderList.add("ARD")
        providedSenderList.add("ARD.Podcast")
        providedSenderList.add("ARTE.DE")
        providedSenderList.add("ARTE.EN")
        providedSenderList.add("ARTE.ES")
        providedSenderList.add("ARTE.FR")
        providedSenderList.add("ARTE.IT")
        providedSenderList.add("ARTE.PL")
        providedSenderList.add("BR")
        providedSenderList.add("DW")
        providedSenderList.add("Funk.net")
        providedSenderList.add("HR")
        providedSenderList.add("KiKa")
        providedSenderList.add("MDR")
        providedSenderList.add("NDR")
        providedSenderList.add("ORF")
        providedSenderList.add("PHOENIX")
        providedSenderList.add("Radio Bremen TV")
        providedSenderList.add("RBB")
        providedSenderList.add("SR")
        providedSenderList.add("SRF")
        providedSenderList.add("SRF.Podcast")
        providedSenderList.add("SWR")
        providedSenderList.add("WDR")
        providedSenderList.add("ZDF")
        providedSenderList.add("ZDF-tivi")

        providedSenderList.sortWith(GermanStringSorter.getInstance())
    }
}