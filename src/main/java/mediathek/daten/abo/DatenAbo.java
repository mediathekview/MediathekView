package mediathek.daten.abo;

import mediathek.tool.GermanStringSorter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamWriter;
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
    private String[] irgendwoFilterPattern;
    private String[] themaFilterPattern;
    private String[] titelFilterPattern;
    private int mindestdauerMinuten;
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
    private String thema = "";
    private String titel = "";
    private String thema_titel = "";
    private String irgendwo = "";
    private String zielpfad = "";
    private String down_datum = ""; //TODO store as date??
    private String pSetName = "";
    /**
     * Whether or not to use minimum film length or maximum film length.
     */
    private FilmLengthState filmLengthState;

    public DatenAbo() {
        // for backward compatibility make it true by default
        filmLengthState = FilmLengthState.MINIMUM;

        mindestdauerMinuten = 0;
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

    public final void setMindestDauerMinuten(int dauer) {
        mindestdauerMinuten = dauer;
    }

    public FilmLengthState getFilmLengthState() {
        return filmLengthState;
    }

    public void setFilmLengthState(FilmLengthState filmLengthState) {
        this.filmLengthState = filmLengthState;
    }

    public String getPsetName() {
        return pSetName;
    }

    public void setPsetName(String pset) {
        this.pSetName = pset;
    }

    public String getDownDatum() {
        return down_datum;
    }

    public void setDownDatum(String datum) {
        this.down_datum = datum;
    }

    public String getZielpfad() {
        return zielpfad;
    }

    public void setZielpfad(String ziel) {
        this.zielpfad = ziel;
    }

    public String getIrgendwo() {
        return irgendwo;
    }

    public void setIrgendwo(String irgendwo) {
        this.irgendwo = irgendwo;
    }

    public String getThemaTitel() {
        return thema_titel;
    }

    public void setThemaTitel(String themaTitel) {
        this.thema_titel = themaTitel;
    }

    public String getTitle() {
        return titel;
    }

    public void setTitle(String titel) {
        this.titel = titel;
    }

    public String getThema() {
        return thema;
    }

    public void setThema(String thema) {
        this.thema = thema;
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
        ret.mindestdauerMinuten = this.mindestdauerMinuten;
        ret.filmLengthState = this.filmLengthState;
        ret.active = this.active;
        ret.name = this.name;
        ret.sender = this.sender;
        ret.thema = this.thema;
        ret.titel = this.titel;
        ret.thema_titel = this.thema_titel;
        ret.down_datum = this.down_datum;
        ret.zielpfad = this.zielpfad;
        ret.pSetName = this.pSetName;
        ret.irgendwo = this.irgendwo;
        return ret;
    }

    /**
     * Liefert TRUE wenn das Abo leer ist, also bei jedem Film ansprechen würde.
     * ist dann offensichtlich falsch!!
     *
     * @return true wenn Abo leer.
     */
    public boolean isInvalid() {
        return getSender().isEmpty()
                && getThema().isEmpty()
                && getTitle().isEmpty()
                && getThemaTitel().isEmpty()
                && getIrgendwo().isEmpty();
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
            writeElement.accept(AboTags.MINDESTDAUER.getXmlName(), Integer.toString(getMindestDauerMinuten()));
            writeElement.accept(AboTags.MIN.getXmlName(), Boolean.toString(getFilmLengthState() == FilmLengthState.MINIMUM));
            writeElement.accept(AboTags.ZIELPFAD.getXmlName(), getZielpfad());
            writeElement.accept(AboTags.DOWN_DATUM.getXmlName(), getDownDatum());
            writeElement.accept(AboTags.PSET.getXmlName(), getPsetName());

            writer.writeEndElement();
            writer.writeCharacters("\n");
        } catch (Exception ex) {
            logger.error("writeToConfig", ex);
        }
    }

    public void readFromConfig(@NotNull XMLStreamReader parser) throws XMLStreamException, AssertionError {
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
                                if (Boolean.parseBoolean(text))
                                    setFilmLengthState(FilmLengthState.MINIMUM);
                                else
                                    setFilmLengthState(FilmLengthState.MAXIMUM);
                                break;

                            case NAME:
                                setName(text);
                                break;

                            case SENDER:
                                setSender(text);
                                break;

                            case THEMA:
                                setThema(text);
                                break;

                            case TITEL:
                                setTitle(text);
                                break;

                            case THEMA_TITEL:
                                setThemaTitel(text);
                                break;

                            case IRGENDWO:
                                setIrgendwo(text);
                                break;

                            case MINDESTDAUER:
                                setMindestDauerMinuten(Integer.parseInt(text));
                                break;

                            case ZIELPFAD:
                                setZielpfad(text);
                                break;

                            case DOWN_DATUM:
                                setDownDatum(text);
                                break;

                            case PSET:
                                setPsetName(text);
                                break;

                            default:
                                throw new AssertionError("Illegal tag detected");
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
