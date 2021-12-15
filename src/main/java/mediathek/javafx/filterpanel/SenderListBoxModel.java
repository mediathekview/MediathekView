package mediathek.javafx.filterpanel;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;

public class SenderListBoxModel {
    public static final EventList<String> SENDER_LIST = new BasicEventList<>();

    static {
        SENDER_LIST.add("3Sat");
        SENDER_LIST.add("ARD");
        SENDER_LIST.add("ARD.Podcast");
        SENDER_LIST.add("ARTE.DE");
        SENDER_LIST.add("ARTE.EN");
        SENDER_LIST.add("ARTE.ES");
        SENDER_LIST.add("ARTE.FR");
        SENDER_LIST.add("ARTE.IT");
        SENDER_LIST.add("ARTE.PL");
        SENDER_LIST.add("BR");
        SENDER_LIST.add("DW");
        SENDER_LIST.add("Funk.net");
        SENDER_LIST.add("HR");
        SENDER_LIST.add("KiKa");
        SENDER_LIST.add("MDR");
        SENDER_LIST.add("NDR");
        SENDER_LIST.add("ORF");
        SENDER_LIST.add("PHOENIX");
        SENDER_LIST.add("Radio Bremen TV");
        SENDER_LIST.add("RBB");
        SENDER_LIST.add("SR");
        SENDER_LIST.add("SRF");
        SENDER_LIST.add("SRF.Podcast");
        SENDER_LIST.add("SWR");
        SENDER_LIST.add("WDR");
        SENDER_LIST.add("ZDF");
        SENDER_LIST.add("ZDF-tivi");
    }
}
