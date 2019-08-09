package mediathek.tool.cellrenderer;

import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.config.Daten;
import mediathek.config.MVColor;
import mediathek.controller.history.SeenHistoryController;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.tool.table.MVTable;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;

@SuppressWarnings("serial")
public class CellRendererFilme extends CellRendererBaseWithStart {
    private static final Logger logger = LogManager.getLogger(CellRendererFilme.class);
    private final Icon selectedStopIcon;
    private final Icon normalStopIcon;
    private final SeenHistoryController history;
    private final Icon selectedDownloadIcon;
    private final Icon normalDownloadIcon;
    private final Icon selectedPlayIcon;
    private final Icon normalPlayIcon;

    public CellRendererFilme(Daten d) {
        super(d.getSenderIconCache());

        selectedDownloadIcon = IconFontSwing.buildIcon(FontAwesome.DOWNLOAD, 16, Color.WHITE);
        normalDownloadIcon = IconFontSwing.buildIcon(FontAwesome.DOWNLOAD, 16);

        selectedPlayIcon = IconFontSwing.buildIcon(FontAwesome.PLAY, 16, Color.WHITE);
        normalPlayIcon = IconFontSwing.buildIcon(FontAwesome.PLAY, 16);

        history = d.getSeenHistoryController();
        selectedStopIcon = IconFontSwing.buildIcon(FontAwesome.STOP, 16, Color.WHITE);
        normalStopIcon = IconFontSwing.buildIcon(FontAwesome.STOP, 16);
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
            setBackground(null);
            setForeground(null);
            setIcon(null);
            setToolTipText(null);
            setHorizontalAlignment(SwingConstants.LEADING);
            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

            final int rowModelIndex = table.convertRowIndexToModel(row);
            final int columnModelIndex = table.convertColumnIndexToModel(column);
            final DatenFilm datenFilm = (DatenFilm) table.getModel().getValueAt(rowModelIndex, DatenFilm.FILM_REF);
            final DatenDownload datenDownload = Daten.getInstance().getListeDownloadsButton().getDownloadUrlFilm(datenFilm.getUrl());

            if (((MVTable) table).isLineBreak()) {
                JTextArea textArea;
                switch (columnModelIndex) {
                    case DatenFilm.FILM_THEMA:
                    case DatenFilm.FILM_TITEL:
                    case DatenFilm.FILM_URL:
                        textArea = new JTextArea();
                        textArea.setLineWrap(true);
                        textArea.setWrapStyleWord(true);
                        textArea.setText(value.toString());
                        textArea.setForeground(getForeground());
                        textArea.setBackground(getBackground());
                        setSelectionFont(textArea, isSelected);
                        setColor(textArea, datenFilm, datenDownload, isSelected);
                        return textArea;
                }
            }

            setSelectionFont(this, isSelected);

            switch (columnModelIndex) {
                case DatenFilm.FILM_NR:
                case DatenFilm.FILM_DATUM:
                case DatenFilm.FILM_ZEIT:
                case DatenFilm.FILM_DAUER:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    break;
                case DatenFilm.FILM_GROESSE:
                    setHorizontalAlignment(SwingConstants.RIGHT);
                    break;
                case DatenFilm.FILM_ABSPIELEN:
                    handleButtonStartColumn(datenDownload, isSelected);
                    break;

                case DatenFilm.FILM_AUFZEICHNEN:
                    handleButtonDownloadColumn(isSelected);
                    break;
                case DatenFilm.FILM_SENDER:
                    if (((MVTable) table).showSenderIcons()) {
                        setSenderIcon((String) value, ((MVTable) table).useSmallSenderIcons);
                    }
                    break;
            }

            setColor(this, datenFilm, datenDownload, isSelected);
        } catch (Exception ex) {
            logger.error("Fehler", ex);
        }
        return this;
    }

    private void setColor(Component c, DatenFilm datenFilm, DatenDownload datenDownload, boolean isSelected) {
        // gestarteter Film
        final boolean start = (datenDownload != null) && (datenDownload.start != null);

        if (start) {
            //film is started for download
            setBackgroundColor(c, datenDownload.start, isSelected);
        } else {
            //not a start, set specific background colors
            if (datenFilm.isLivestream()) {
                // bei livestreams keine History anzeigen
                c.setForeground(MVColor.FILM_LIVESTREAM.color);
            } else if (history.urlPruefen(datenFilm.getUrl())) {
                if (!isSelected) {
                    c.setBackground(MVColor.FILM_HISTORY.color);
                }
            } else if (datenFilm.isNew()) {
                // fix #259
                if (!isSelected)
                    c.setForeground(MVColor.FILM_NEU.color);
            }

            if (geoMelden)
                setupGeoblockingBackground(c, datenFilm.getGeo(), isSelected);
        }
    }

    private void handleButtonStartColumn(final DatenDownload datenDownload, final boolean isSelected) {
        // Button Abspielen
        setHorizontalAlignment(SwingConstants.CENTER);
        if (datenDownload != null) {
            if (datenDownload.start != null) {
                if (datenDownload.start.status == Start.STATUS_RUN) {
                    setToolTipText("Film stoppen");
                    final Icon icon;
                    if (isSelected)
                        icon = selectedStopIcon;
                    else
                        icon = normalStopIcon;

                    setIcon(icon);
                }
            }
        }

        if (getIcon() == null) {
            setToolTipText("Film abspielen");
            final Icon icon;
            if (isSelected)
                icon = selectedPlayIcon;
            else
                icon = normalPlayIcon;

            setIcon(icon);
        }
    }

    private void handleButtonDownloadColumn(final boolean isSelected) {
        // Button Aufzeichnen
        setHorizontalAlignment(SwingConstants.CENTER);
        setToolTipText("Film aufzeichnen");
        final Icon icon;
        if (isSelected)
            icon = selectedDownloadIcon;
        else
            icon = normalDownloadIcon;

        setIcon(icon);
    }
}
