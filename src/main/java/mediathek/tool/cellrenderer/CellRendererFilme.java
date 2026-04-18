package mediathek.tool.cellrenderer;

import mediathek.config.Daten;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.swing.IconUtils;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.table.MVTable;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jspecify.annotations.NonNull;
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid;
import org.kordamp.ikonli.swing.FontIcon;

import javax.swing.*;
import java.awt.*;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

public class CellRendererFilme extends CellRendererBaseWithStart {
    private static final Logger logger = LogManager.getLogger(CellRendererFilme.class);
    private static final DateTimeFormatter PARSER = DateTimeFormatter.ofPattern("H:mm[:ss]");
    private static final DateTimeFormatter SHORT = DateTimeFormatter.ofPattern("HH:mm");
    private static final DateTimeFormatter LONG = DateTimeFormatter.ofPattern("HH:mm:ss");
    private final FontIcon selectedStopIcon;
    private final FontIcon normalStopIcon;
    private final FontIcon selectedDownloadIcon;
    private final FontIcon normalDownloadIcon;
    private final FontIcon selectedPlayIcon;
    private final FontIcon normalPlayIcon;
    private final FontIcon selectedBookmarkIcon;
    private final FontIcon normalBookmarkIcon;
    private final FontIcon selectedBookmarkIconHighlighted;

    public CellRendererFilme() {
        selectedDownloadIcon = FontIcon.of(FontAwesomeSolid.DOWNLOAD, IconUtils.DEFAULT_SIZE, Color.WHITE);
        normalDownloadIcon = IconUtils.of(FontAwesomeSolid.DOWNLOAD);

        selectedPlayIcon = FontIcon.of(FontAwesomeSolid.PLAY, IconUtils.DEFAULT_SIZE, Color.WHITE);
        normalPlayIcon = IconUtils.of(FontAwesomeSolid.PLAY);

        selectedStopIcon = FontIcon.of(FontAwesomeSolid.STOP, IconUtils.DEFAULT_SIZE, Color.WHITE);
        normalStopIcon = IconUtils.of(FontAwesomeSolid.STOP);

        selectedBookmarkIcon = FontIcon.of(FontAwesomeSolid.BOOKMARK, IconUtils.DEFAULT_SIZE, Color.WHITE);
        selectedBookmarkIconHighlighted = FontIcon.of(FontAwesomeSolid.BOOKMARK, IconUtils.DEFAULT_SIZE, Color.ORANGE);
        normalBookmarkIcon = IconUtils.of(FontAwesomeSolid.BOOKMARK);
    }

    private JTextArea createTextArea(String content) {
        var textArea = new JTextArea();
        textArea.setLineWrap(true);
        textArea.setWrapStyleWord(true);
        textArea.setText(content);
        textArea.setForeground(getForeground());
        textArea.setBackground(getBackground());

        var fontSize = textArea.getFont().getSize2D();
        var labelFont = UIManager.getFont("Label.font");
        textArea.setFont(labelFont.deriveFont(fontSize));

        return textArea;
    }

    @Override
    public Component getTableCellRendererComponent(
            JTable table,
            Object value,
            boolean isSelected,
            boolean hasFocus,
            int row,
            int column) {
        try {
            resetComponent();
            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

            final int rowModelIndex = table.convertRowIndexToModel(row);
            final int columnModelIndex = table.convertColumnIndexToModel(column);
            final DatenFilm datenFilm = (DatenFilm) table.getModel().getValueAt(rowModelIndex, DatenFilm.FILM_REF);
            final var mvTable = (MVTable) table;


            //shortcut if we want to have line breaks, use text areas and skip the rest
            if (mvTable.isLineBreak()) {
                setHorizontalAlignment(SwingConstants.LEFT);
                setVerticalAlignment(SwingConstants.TOP);

                switch (columnModelIndex) {
                    case DatenFilm.FILM_THEMA, DatenFilm.FILM_TITEL, DatenFilm.FILM_URL -> {
                        return createTextArea(value.toString());
                    }
                }
            }
            else
                applyHorizontalAlignment(columnModelIndex);

            //NOT IN LINEBREAK MODE -> REGULAR HANDLING
            //here comes the content...
            switch (columnModelIndex) {
                case DatenFilm.FILM_DAUER -> setText(datenFilm.getFilmLengthAsString());
                case DatenFilm.FILM_ABSPIELEN -> {
                    final DatenDownload datenDownload = Daten.getInstance().getListeDownloadsButton().getDownloadUrlFilm(datenFilm.getUrlNormalQuality());
                    handleButtonStartColumn(datenDownload, isSelected);
                }
                case DatenFilm.FILM_AUFZEICHNEN -> handleButtonDownloadColumn(isSelected);
                case DatenFilm.FILM_MERKEN ->
                        handleButtonBookmarkColumn(datenFilm.isBookmarked(), isSelected, datenFilm.isLivestream());
                case DatenFilm.FILM_SENDER -> {
                    if (mvTable.showSenderIcons()) {
                        Dimension targetDim = getSenderCellDimension(table, row, column);
                        setSenderIcon(value.toString(), targetDim, isSelected);
                    }
                }
                case DatenFilm.FILM_TITEL -> {
                    setText(datenFilm.getTitle());
                    setIndicatorIcons(table, datenFilm, isSelected);
                }
                case DatenFilm.FILM_GEO -> drawGeolocationIcons(datenFilm, isSelected);

                case DatenFilm.FILM_ZEIT -> drawTime(datenFilm);
            }
        }
        catch (Exception ex) {
            logger.error("Fehler", ex);
        }

        return this;
    }

    /**
     * Draw time without trailing seconds if zero.
     *
     * @param film input film object.
     */
    private void drawTime(@NonNull DatenFilm film) {
        var zeit = film.getSendeZeit();
        if (zeit == null || zeit.isBlank()) {
            setText("");
            return;
        }

        zeit = zeit.trim();
        try {
            var t = LocalTime.parse(zeit, PARSER);
            var longFormat = ApplicationConfiguration.getConfiguration()
                    .getBoolean(ApplicationConfiguration.UI_TAB_FILME_TIME_USE_LONG_FORMAT, false);
            setText((longFormat ? LONG : SHORT).format(t));
        }
        catch (DateTimeParseException ex) {
            setText(zeit);
        }
    }

    /**
     * Apply the specific horizontal alignment to the cell based on column
     *
     * @param columnModelIndex the current column index
     */
    private void applyHorizontalAlignment(final int columnModelIndex) {
        switch (columnModelIndex) {
            case DatenFilm.FILM_NR, DatenFilm.FILM_DATUM, DatenFilm.FILM_ZEIT, DatenFilm.FILM_DAUER,
                 DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN, DatenFilm.FILM_MERKEN ->
                    setHorizontalAlignment(SwingConstants.CENTER);
            case DatenFilm.FILM_GROESSE -> setHorizontalAlignment(SwingConstants.RIGHT);
        }
    }

    private void handleButtonStartColumn(final DatenDownload datenDownload, final boolean isSelected) {
        // Button Abspielen
        if (datenDownload != null) {
            if (datenDownload.start != null) {
                if (datenDownload.start.status == Start.STATUS_RUN) {
                    setIconAndToolTip(isSelected, normalStopIcon, selectedStopIcon, "Film stoppen");
                }
            }
        }

        if (getIcon() == null) {
            setIconAndToolTip(isSelected, normalPlayIcon, selectedPlayIcon, "Film abspielen");
        }
    }

    private void setIconAndToolTip(boolean isSelected, Icon normal, Icon selected, String text) {
        setToolTipText(text);
        setIcon(isSelected ? selected : normal);
    }

    private void handleButtonDownloadColumn(final boolean isSelected) {
        // Button Aufzeichnen
        setIconAndToolTip(isSelected, normalDownloadIcon, selectedDownloadIcon, "Film aufzeichnen");
    }

    private void handleButtonBookmarkColumn(final boolean isBookMarked, final boolean isSelected, boolean isLivestream) {
        if (isLivestream) {
            setIcon(null);
            setToolTipText("");
        }
        else {
            // Button Merken
            setToolTipText(isBookMarked ? "Film aus Merkliste entfernen" : "Film merken");
            if (isBookMarked) {
                setIcon(selectedBookmarkIconHighlighted);
            }
            else {
                if (isSelected) {
                    setIcon(selectedBookmarkIcon);
                }
                else
                    setIcon(normalBookmarkIcon);
            }
        }
    }
}
