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
import java.util.Optional;
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
    public static final int MAX_ELEM = 13;
    public static final String TAG = "Abonnement";
    private static final Logger logger = LogManager.getLogger(DatenAbo.class);
    private static final GermanStringSorter sorter = GermanStringSorter.getInstance();
    public static boolean[] spaltenAnzeigen = new boolean[MAX_ELEM];
    public int mindestdauerMinuten;
    public String[] arr;
    public String[] titel, thema, irgendwo;
    private boolean min;
    private int nr;

    public DatenAbo() {
        initialize();
    }

    public DatenAbo(String name, String sender, String thema, String titel, String themaTitel, String irgendwo, int mmindestdauerMinuten, boolean min, String ziel, String pset) {
        initialize();
        setName(name);
        setSender(sender);
        setThema(thema);
        setTitle(titel);
        setThemaTitel(themaTitel);
        setIrgendwo(irgendwo);
        setMindestDauerMinuten(mmindestdauerMinuten);

        setMin(min);

        setZielpfad(ziel);
        setPsetName(pset);
    }

    public static boolean anzeigen(int i) {
        return spaltenAnzeigen == null || spaltenAnzeigen[i];
    }

    public boolean getMin() {
        return min;
    }

    public void setMin(boolean min) {
        arr[ABO_MIN] = Boolean.toString(min);
        this.min = min;
    }

    public String getPsetName() {
        return arr[ABO_PSET];
    }

    public void setPsetName(String pset) {
        arr[ABO_PSET] = pset;
    }

    public String getDownDatum() {
        return arr[ABO_DOWN_DATUM];
    }

    public void setDownDatum(String datum) {
        arr[ABO_DOWN_DATUM] = datum;
    }

    public String getZielpfad() {
        return arr[ABO_ZIELPFAD];
    }

    public void setZielpfad(String ziel) {
        arr[ABO_ZIELPFAD] = ziel;
    }

    public String getMindestDauer() {
        return arr[ABO_MINDESTDAUER];
    }

    public String getIrgendwo() {
        return arr[ABO_IRGENDWO];
    }

    public void setIrgendwo(String irgendwo) {
        arr[ABO_IRGENDWO] = irgendwo;
    }

    public String getThemaTitel() {
        return arr[ABO_THEMA_TITEL];
    }

    public void setThemaTitel(String themaTitel) {
        arr[ABO_THEMA_TITEL] = themaTitel;
    }

    public String getTitle() {
        return arr[ABO_TITEL];
    }

    public void setTitle(String titel) {
        arr[ABO_TITEL] = titel;
    }

    public String getThema() {
        return arr[ABO_THEMA];
    }

    public void setThema(String thema) {
        arr[ABO_THEMA] = thema;
    }

    public String getSender() {
        return arr[ABO_SENDER];
    }

    public void setSender(String sender) {
        arr[ABO_SENDER] = sender;
    }

    public String getName() {
        return arr[ABO_NAME];
    }

    public void setName(String name) {
        arr[ABO_NAME] = name;
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
        return getSender().isEmpty()
                && getThema().isEmpty()
                && getTitle().isEmpty()
                && getThemaTitel().isEmpty()
                && getIrgendwo().isEmpty();
    }

    public final void setMindestDauerMinuten(int d) {
        mindestdauerMinuten = d;
        arr[ABO_MINDESTDAUER] = String.valueOf(d);
    }

    public void setMindestDauerMinuten() {
        if (getMindestDauer().isEmpty()) {
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

    /**
     * Returns whether this item is active or not.
     *
     * @return true if it active, false otherwise
     */
    public boolean isActive() {
        return Boolean.parseBoolean(arr[DatenAbo.ABO_EINGESCHALTET]);
    }

    public void setActive(boolean active) {
        if (active)
            activate();
        else
            arr[DatenAbo.ABO_EINGESCHALTET] = String.valueOf(false);
    }

    /**
     * Set abo state to active.
     */
    private void activate() {
        arr[DatenAbo.ABO_EINGESCHALTET] = String.valueOf(true);
    }

    private void initialize() {
        arr = new String[MAX_ELEM];
        Arrays.fill(arr, "");
        // neue Abos sind immer ein
        activate();

        // for backward compatibility make it true by default
        setMin(true);
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
            } catch (XMLStreamException e) {
                logger.error("writeElement failed", e);
            }
        };

        try {
            writer.writeStartElement(TAG);
            writer.writeCharacters("\n");

            //never write ABO_NR
            writeElement.accept(AboTags.EINGESCHALTET.xml_name, arr[ABO_EINGESCHALTET]);
            writeElement.accept(AboTags.NAME.xml_name, getName());
            writeElement.accept(AboTags.SENDER.xml_name, getSender());
            writeElement.accept(AboTags.THEMA.xml_name, getThema());
            writeElement.accept(AboTags.TITEL.xml_name, getTitle());
            writeElement.accept(AboTags.THEMA_TITEL.xml_name, getThemaTitel());
            writeElement.accept(AboTags.IRGENDWO.xml_name, getIrgendwo());
            writeElement.accept(AboTags.MINDESTDAUER.xml_name, getMindestDauer());
            writeElement.accept(AboTags.MIN.xml_name, Boolean.toString(getMin()));
            writeElement.accept(AboTags.ZIELPFAD.xml_name, getZielpfad());
            writeElement.accept(AboTags.DOWN_DATUM.xml_name, getDownDatum());
            writeElement.accept(AboTags.PSET.xml_name, getPsetName());

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
                AboTags.fromXmlTag(parser.getLocalName()).ifPresent(tag -> {
                    try {
                        final var text = parser.getElementText();
                        if (tag == AboTags.MIN) {
                            setMin(Boolean.parseBoolean(text));
                        } else {
                            arr[tag.index] = text;
                        }
                    }
                    catch (XMLStreamException e) {
                        logger.error("Error reading abo entry", e);
                    }
                });
            }
        }
    }

    @Override
    public int compareTo(@NotNull DatenAbo other) {
        return sorter.compare(getName(), other.getName());
    }

    enum AboTags {
        NR(ABO_NR, "Nr"),
        EINGESCHALTET(ABO_EINGESCHALTET, "aktiv"),
        NAME(ABO_NAME, "Name"),
        SENDER(ABO_SENDER, "Sender"),
        THEMA(ABO_THEMA, "Thema"),
        TITEL(ABO_TITEL, "Titel"),
        THEMA_TITEL(ABO_THEMA_TITEL, "Thema-Titel"),
        IRGENDWO(ABO_IRGENDWO, "Irgendwo"),
        MINDESTDAUER(ABO_MINDESTDAUER, "Mindestdauer"),
        MIN(ABO_MIN, "min_max"),
        ZIELPFAD(ABO_ZIELPFAD, "Zielpfad"),
        DOWN_DATUM(ABO_DOWN_DATUM, "letztes_Abo"),
        PSET(ABO_PSET, "Programmset");

        private final int index;
        private final String xml_name;

        AboTags(int index, String xml_name) {
            this.index = index;
            this.xml_name = xml_name;
        }

        public static Optional<AboTags> fromXmlTag(@NotNull String tag) {
            return Arrays.stream(AboTags.values()).filter(e -> e.xml_name.equals(tag)).findAny();
        }
    }
}
