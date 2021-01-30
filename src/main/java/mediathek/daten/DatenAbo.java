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
    public static final int ABO_REF = 13;
    public static final String[] COLUMN_NAMES = {"Nr", "aktiv", "Name",
            "Sender", "Thema", "Titel", "Thema-Titel",
            "Irgendwo", "Dauer", "min/max", "Zielpfad", "letztes Abo", "Programmset", ""};
    public static final int MAX_ELEM = 14;
    public static final String TAG = "Abonnement";
    private static final Logger logger = LogManager.getLogger(DatenAbo.class);
    private static final GermanStringSorter sorter = GermanStringSorter.getInstance();
    public static boolean[] spaltenAnzeigen = new boolean[MAX_ELEM];
    private final String[] arr = new String[MAX_ELEM];
    private String[] irgendwoFilterPattern;
    private String[] themaFilterPattern;
    private String[] titelFilterPattern;
    private int mindestdauerMinuten;
    private boolean min;
    /**
     * Used internally for display in table.
     * Should NOT be used in code logic!!
     */
    private int nr;
    /**
     * Stores the active state of the abo.
     * On by default.
     */
    private boolean active = true;
    /**
     * The display name.
     */
    private String name = "";
    private String sender = "";

    public DatenAbo() {
        Arrays.fill(arr, "");

        // for backward compatibility make it true by default
        setMin(true);

        mindestdauerMinuten = 0;
        arr[ABO_MINDESTDAUER] = "0";
    }

    public static boolean anzeigen(int i) {
        return spaltenAnzeigen == null || spaltenAnzeigen[i];
    }

    public String[] getTitelFilterPattern() {
        return titelFilterPattern;
    }

    public void setTitelFilterPattern(String[] titelFilterPattern) {
        this.titelFilterPattern = titelFilterPattern;
    }

    public String[] getThemaFilterPattern() {
        return themaFilterPattern;
    }

    public void setThemaFilterPattern(String[] themaFilterPattern) {
        this.themaFilterPattern = themaFilterPattern;
    }

    public String[] getIrgendwoFilterPattern() {
        return irgendwoFilterPattern;
    }

    public void setIrgendwoFilterPattern(String[] irgendwoFilterPattern) {
        this.irgendwoFilterPattern = irgendwoFilterPattern;
    }

    public int getMindestDauerMinuten() {
        return mindestdauerMinuten;
    }

    public final void setMindestDauerMinuten(int d) {
        mindestdauerMinuten = d;
        arr[ABO_MINDESTDAUER] = String.valueOf(d);
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
        return sender;
    }

    public void setSender(String sender) {
        this.sender = sender;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getNr() {
        return nr;
    }

    public void setNr(int nr) {
        this.nr = nr;
    }

    public DatenAbo getCopy() {
        //FIXME do it correct!
        DatenAbo ret = new DatenAbo();
        System.arraycopy(this.arr, 0, ret.arr, 0, arr.length);
        ret.mindestdauerMinuten = this.mindestdauerMinuten;
        ret.min = this.min;
        ret.active = this.active;
        ret.name = this.name;
        ret.sender = this.sender;
        return ret;
    }

    /**
     * Liefert TRUE wenn das Abo leer ist, also bei jedem Film ansprechen würde.
     * ist dann offensichtlich falsch!!
     *
     * @return true wenn Abo leer.
     */
    public boolean isEmpty() {
        return getSender().isEmpty()
                && getThema().isEmpty()
                && getTitle().isEmpty()
                && getThemaTitel().isEmpty()
                && getIrgendwo().isEmpty();
    }

    public void setMindestDauerMinuten() {
        //TODO remove after accesses to arr[] have been killed
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
        return active;
    }

    public void setActive(boolean active) {
        this.active = active;
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
            writeElement.accept(AboTags.EINGESCHALTET.getXmlName(), Boolean.toString(isActive()));
            writeElement.accept(AboTags.NAME.getXmlName(), getName());
            writeElement.accept(AboTags.SENDER.getXmlName(), getSender());
            writeElement.accept(AboTags.THEMA.getXmlName(), getThema());
            writeElement.accept(AboTags.TITEL.getXmlName(), getTitle());
            writeElement.accept(AboTags.THEMA_TITEL.getXmlName(), getThemaTitel());
            writeElement.accept(AboTags.IRGENDWO.getXmlName(), getIrgendwo());
            writeElement.accept(AboTags.MINDESTDAUER.getXmlName(), getMindestDauer());
            writeElement.accept(AboTags.MIN.getXmlName(), Boolean.toString(getMin()));
            writeElement.accept(AboTags.ZIELPFAD.getXmlName(), getZielpfad());
            writeElement.accept(AboTags.DOWN_DATUM.getXmlName(), getDownDatum());
            writeElement.accept(AboTags.PSET.getXmlName(), getPsetName());

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
                        switch (tag) {
                            case EINGESCHALTET:
                                setActive(Boolean.parseBoolean(text));
                                break;

                            case MIN:
                                setMin(Boolean.parseBoolean(text));
                                break;

                            case NAME:
                                setName(text);
                                break;

                            case SENDER:
                                setSender(text);
                                break;

                            default:
                                arr[tag.getIndex()] = text;
                                break;
                        }
                    } catch (XMLStreamException e) {
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

}
