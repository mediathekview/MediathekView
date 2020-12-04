package mediathek.daten;

import mediathek.tool.GermanStringSorter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamWriter;
import java.util.Arrays;
import java.util.function.BiConsumer;

public class DatenAbo implements Comparable<DatenAbo> {

    public static final int ABO_NR = 0;
    public static final int ABO_EINGESCHALTET = 1;
    public static final int ABO_NAME = 2;
    public static final int ABO_SENDER = 3;
    public static final int ABO_THEMA = 4;
    public static final int ABO_TITEL = 5;
    public static final int ABO_THEMA_TITEL = 6;
    public static final int ABO_IRGENDWO = 7;
    public static final int ABO_MINDESTDAUER = 8;
    public static final int ABO_MIN = 9;
    public static final int ABO_ZIELPFAD = 10;
    public static final int ABO_DOWN_DATUM = 11;
    public static final int ABO_PSET = 12;
    public static final String[] COLUMN_NAMES = {"Nr", "aktiv", "Name",
            "Sender", "Thema", "Titel", "Thema-Titel",
            "Irgendwo", "Dauer", "min/max", "Zielpfad", "letztes Abo", "Programmset"};
    private static final String[] XML_NAMES = {"Nr", "aktiv", "Name",
            "Sender", "Thema", "Titel", "Thema-Titel",
            "Irgendwo", "Mindestdauer", "min_max", "Zielpfad", "letztes_Abo", "Programmset"};

    public static final int MAX_ELEM = 13;
    public static final String TAG = "Abonnement";
    private static final Logger logger = LogManager.getLogger(DatenAbo.class);
    private static final GermanStringSorter sorter = GermanStringSorter.getInstance();
    public static boolean[] spaltenAnzeigen = new boolean[MAX_ELEM];
    public int mindestdauerMinuten;
    public boolean min = true;
    public String[] arr;
    public String[] titel, thema, irgendwo;
    private int nr;

    public DatenAbo() {
        initialize();
    }

    public DatenAbo(String name, String sender, String thema, String titel, String themaTitel, String irgendwo, int mmindestdauerMinuten, boolean min, String ziel, String pset) {
        initialize();
        arr[ABO_NAME] = name;
        arr[ABO_SENDER] = sender;
        arr[ABO_THEMA] = thema;
        arr[ABO_TITEL] = titel;
        arr[ABO_THEMA_TITEL] = themaTitel;
        arr[ABO_IRGENDWO] = irgendwo;
        setMindestDauerMinuten(mmindestdauerMinuten);
        arr[ABO_MIN] = Boolean.toString(min);
        this.min = min;
        arr[ABO_ZIELPFAD] = ziel;
        arr[ABO_PSET] = pset;
    }

    public static boolean anzeigen(int i) {
        return spaltenAnzeigen == null || spaltenAnzeigen[i];
    }

    public int getNr() {
        return nr;
    }

    public void setNr(int nr) {
        this.nr = nr;
    }

    public DatenAbo getCopy() {
        DatenAbo ret = new DatenAbo();
        System.arraycopy(this.arr, 0, ret.arr, 0, arr.length);
        ret.mindestdauerMinuten = this.mindestdauerMinuten;
        ret.min = this.min;
        return ret;
    }

    public boolean isEmpty() {
        //liefert TRUE wenn das Abo leer ist, also bei jedem Film ansprechen würde
        //ist dann offensichtlich falsch!!
        return arr[ABO_SENDER].isEmpty()
                && arr[ABO_THEMA].isEmpty()
                && arr[ABO_TITEL].isEmpty()
                && arr[ABO_THEMA_TITEL].isEmpty()
                && arr[ABO_IRGENDWO].isEmpty();
    }

    public final void setMindestDauerMinuten(int d) {
        mindestdauerMinuten = d;
        arr[ABO_MINDESTDAUER] = String.valueOf(d);
    }

    public void setMindestDauerMinuten() {
        if (this.arr[DatenAbo.ABO_MINDESTDAUER].isEmpty()) {
            // für den ProgUpdate
            mindestdauerMinuten = 0;
            arr[ABO_MINDESTDAUER] = "0";
        }
        try {
            mindestdauerMinuten = Integer.parseInt(this.arr[DatenAbo.ABO_MINDESTDAUER]);
        } catch (Exception ex) {
            logger.error("setMindestDauerMinuten()", ex);
            mindestdauerMinuten = 0;
            arr[ABO_MINDESTDAUER] = "0";
        }
    }

    public boolean aboIstEingeschaltet() {
        if (arr[DatenAbo.ABO_EINGESCHALTET].isEmpty()) {
            aboEin();
            return true;
        }
        return Boolean.parseBoolean(arr[DatenAbo.ABO_EINGESCHALTET]);
    }

    private void aboEin() {
        arr[DatenAbo.ABO_EINGESCHALTET] = String.valueOf(true);
    }

    private void initialize() {
        arr = new String[MAX_ELEM];
        Arrays.fill(arr, "");
        // neue Abos sind immer ein
        aboEin();
    }

    /**
     * Write all data to config.
     *
     * @param writer the writer used.
     */
    public void writeToConfig(@NotNull XMLStreamWriter writer) {
        final BiConsumer<String, String> writeElement = (tagName, content) -> {
            try {
                writer.writeCharacters("\t");
                writer.writeStartElement(tagName);
                writer.writeCharacters(content);
                writer.writeEndElement();
                writer.writeCharacters("\n");
            }
            catch (XMLStreamException e) {
                logger.error("writeElement failed", e);
            }
        };

        try {
            writer.writeStartElement(TAG);
            writer.writeCharacters("\n");

            //never write ABO_NR
            writeElement.accept(XML_NAMES[ABO_EINGESCHALTET], arr[ABO_EINGESCHALTET]);
            writeElement.accept(XML_NAMES[ABO_NAME], arr[ABO_NAME]);
            writeElement.accept(XML_NAMES[ABO_SENDER], arr[ABO_SENDER]);
            writeElement.accept(XML_NAMES[ABO_THEMA], arr[ABO_THEMA]);
            writeElement.accept(XML_NAMES[ABO_TITEL], arr[ABO_TITEL]);
            writeElement.accept(XML_NAMES[ABO_THEMA_TITEL], arr[ABO_THEMA_TITEL]);
            writeElement.accept(XML_NAMES[ABO_IRGENDWO], arr[ABO_IRGENDWO]);
            writeElement.accept(XML_NAMES[ABO_MINDESTDAUER], arr[ABO_MINDESTDAUER]);
            writeElement.accept(XML_NAMES[ABO_MIN], arr[ABO_MIN]);
            writeElement.accept(XML_NAMES[ABO_ZIELPFAD], arr[ABO_ZIELPFAD]);
            writeElement.accept(XML_NAMES[ABO_DOWN_DATUM], arr[ABO_DOWN_DATUM]);
            writeElement.accept(XML_NAMES[ABO_PSET], arr[ABO_PSET]);

            writer.writeEndElement();
            writer.writeCharacters("\n");
        } catch (Exception ex) {
            logger.error("writeToConfig", ex);
        }
    }

    public void readFromConfig(@NotNull XMLStreamReader parser) throws XMLStreamException {
        while (parser.hasNext()) {
            final int event = parser.next();
            if (event == XMLStreamConstants.END_ELEMENT) {
                if (parser.getLocalName().equals(TAG)) {
                    break;
                }
            }
            if (event == XMLStreamConstants.START_ELEMENT) {
                for (int i = 0; i < arr.length; ++i) {
                    if (parser.getLocalName().equals(XML_NAMES[i])) {
                        arr[i] = parser.getElementText();
                        break;
                    }
                }
            }
        }
    }

    @Override
    public int compareTo(@NotNull DatenAbo arg0) {
        return sorter.compare(arr[ABO_NAME], arg0.arr[ABO_NAME]);
    }
}
