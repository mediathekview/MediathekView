package mediathek.tool.cellrenderer;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import mediathek.config.Daten;
import mediathek.config.MVColor;
import mediathek.controller.history.SeenHistoryController;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.tool.SVGIconUtilities;
import mediathek.tool.table.MVTable;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

public class CellRendererFilme extends CellRendererBaseWithStart {
    private static final Logger logger = LogManager.getLogger(CellRendererFilme.class);
    private final FlatSVGIcon selectedStopIcon;
    private final FlatSVGIcon normalStopIcon;
    private final SeenHistoryController history = new SeenHistoryController();
    private final FlatSVGIcon selectedDownloadIcon;
    private final FlatSVGIcon normalDownloadIcon;
    private final FlatSVGIcon selectedPlayIcon;
    private final FlatSVGIcon normalPlayIcon;
    private final FlatSVGIcon selectedBookmarkIcon;
    private final FlatSVGIcon normalBookmarkIcon;
    private final FlatSVGIcon selectedBookmarkIconHighlighted;

    public CellRendererFilme() {
        var whiteColorFilter = new FlatSVGIcon.ColorFilter(color -> Color.WHITE);

        selectedDownloadIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/download.svg");
        selectedDownloadIcon.setColorFilter(whiteColorFilter);

        normalDownloadIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/download.svg");

        selectedPlayIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/play.svg");
        selectedPlayIcon.setColorFilter(whiteColorFilter);

        normalPlayIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/play.svg");

        selectedStopIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/stop.svg");
        selectedStopIcon.setColorFilter(whiteColorFilter);

        normalStopIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/stop.svg");

        selectedBookmarkIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/bookmark.svg");
        selectedBookmarkIcon.setColorFilter(whiteColorFilter);

        selectedBookmarkIconHighlighted = SVGIconUtilities.createSVGIcon("icons/fontawesome/bookmark.svg");
        selectedBookmarkIconHighlighted.setColorFilter(new FlatSVGIcon.ColorFilter(color -> Color.ORANGE));

        normalBookmarkIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/bookmark.svg");
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
            final DatenDownload datenDownload = Daten.getInstance().getListeDownloadsButton().getDownloadUrlFilm(datenFilm.getUrlNormalQuality());
            final boolean isBookMarked = datenFilm.isBookmarked();
            final var mvTable = (MVTable) table;

            setFont((mvTable.getDefaultFont()));

            //shortcut if we want to have line breaks, use text areas and skip the rest
            if (mvTable.isLineBreak()) {
                switch (columnModelIndex) {
                    case DatenFilm.FILM_THEMA, DatenFilm.FILM_TITEL, DatenFilm.FILM_URL -> {
                        var textArea = createTextArea(value.toString());
                        applyColorSettings(textArea, datenFilm, datenDownload, isSelected, isBookMarked);
                        return textArea;
                    }
                }
            }

            applyHorizontalAlignment(columnModelIndex);

            //here comes the content...
            switch (columnModelIndex) {
                case DatenFilm.FILM_DAUER:
                    setText(datenFilm.getDauer());
                    break;

                case DatenFilm.FILM_ABSPIELEN:
                    handleButtonStartColumn(datenDownload, isSelected);
                    break;

                case DatenFilm.FILM_AUFZEICHNEN:
                    handleButtonDownloadColumn(isSelected);
                    break;

                case DatenFilm.FILM_MERKEN:
                    handleButtonBookmarkColumn(isBookMarked, isSelected, datenFilm.isLivestream());
                    break;

                case DatenFilm.FILM_SENDER:
                    if (mvTable.showSenderIcons()) {
                        Dimension targetDim = getSenderCellDimension(table, row, columnModelIndex);
                        setSenderIcon(value.toString(), targetDim);
                    }
                    break;

                case DatenFilm.FILM_TITEL:
                    var title = datenFilm.getTitle();
                    var columnWidth = table.getColumnModel().getColumn(columnModelIndex).getWidth();
                    if (columnWidth < table.getFontMetrics(table.getFont()).stringWidth(title))
                        setToolTipText(title);
                    break;
            }

            applyColorSettings(this, datenFilm, datenDownload, isSelected, isBookMarked);
        } catch (Exception ex) {
            logger.error("Fehler", ex);
        }

        return this;
    }

    /**
     * Apply the specific horizontal alignment to the cell based on column
     *
     * @param columnModelIndex the current column index
     */
    private void applyHorizontalAlignment(final int columnModelIndex) {
        switch (columnModelIndex) {
            case DatenFilm.FILM_NR, DatenFilm.FILM_DATUM, DatenFilm.FILM_ZEIT, DatenFilm.FILM_DAUER, DatenFilm.FILM_ABSPIELEN, DatenFilm.FILM_AUFZEICHNEN, DatenFilm.FILM_MERKEN ->
                    setHorizontalAlignment(SwingConstants.CENTER);
            case DatenFilm.FILM_GROESSE -> setHorizontalAlignment(SwingConstants.RIGHT);
        }
    }

    private void applyColorSettings(Component c, @NotNull DatenFilm datenFilm, DatenDownload datenDownload, boolean isSelected, boolean isBookMarked) {
        // gestarteter Film
        final boolean start = (datenDownload != null) && (datenDownload.start != null);
        final boolean hasBeenSeen = history.hasBeenSeen(datenFilm);

        if (start) {
            //film is started for download
            setBackgroundColor(c, datenDownload.start, isSelected);
        } else {
            //not a start, set specific background colors
            if (datenFilm.isLivestream()) {
                // bei livestreams keine History anzeigen
                c.setForeground(MVColor.FILM_LIVESTREAM.color);
            } else if (hasBeenSeen) {
                if (!isSelected) {
                    c.setBackground(MVColor.FILM_HISTORY.color);
                }
            } else if (datenFilm.isNew()) {
                // fix #259
                if (!isSelected)
                    c.setForeground(MVColor.FILM_NEU.color);
            } else if (isBookMarked && !isSelected) {
                c.setBackground(MVColor.FILM_BOOKMARKED.color);
            }

            if (geoMelden) {
                //only apply geo block colors when we haven´t changed the background for seen history
                if (!hasBeenSeen) {
                    setupGeoblockingBackground(c, datenFilm.getGeo().orElse(""), isSelected);
                }
            }
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
        Icon icon;
        if (isSelected)
            icon = selected;
        else
            icon = normal;

        setIcon(icon);
    }

    private void handleButtonDownloadColumn(final boolean isSelected) {
        // Button Aufzeichnen
        setIconAndToolTip(isSelected, normalDownloadIcon, selectedDownloadIcon, "Film aufzeichnen");
    }

    private void handleButtonBookmarkColumn(final boolean isBookMarked, final boolean isSelected, boolean isLivestream) {
        if (isLivestream) {
            setIcon(null);
            setToolTipText("");
        } else {
            // Button Merken
            setToolTipText(isBookMarked ? "Film aus Merkliste entfernen" : "Film merken");
            if (isBookMarked) {
                if (isSelected) {
                    setIcon(selectedBookmarkIconHighlighted);
                } else
                    setIcon(selectedBookmarkIconHighlighted);
            } else {
                if (isSelected) {
                    setIcon(selectedBookmarkIcon);
                } else
                    setIcon(normalBookmarkIcon);
            }
        }
    }
}
