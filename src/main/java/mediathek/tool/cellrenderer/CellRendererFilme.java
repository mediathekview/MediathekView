/*
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool.cellrenderer;

import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mediathek.config.Daten;
import mediathek.config.MVColor;
import mediathek.controller.MVUsedUrls;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.tool.MVSenderIconCache;
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
    private final MVUsedUrls history;
    private final Icon selectedDownloadIcon;
    private final Icon normalDownloadIcon;
    private final Icon selectedPlayIcon;
    private final Icon normalPlayIcon;

    public CellRendererFilme(Daten d, MVSenderIconCache cache) {
        super(cache);

        selectedDownloadIcon = IconFontSwing.buildIcon(FontAwesome.DOWNLOAD, 16, new Color(255, 255, 255));
        normalDownloadIcon = IconFontSwing.buildIcon(FontAwesome.DOWNLOAD, 16);

        selectedPlayIcon = IconFontSwing.buildIcon(FontAwesome.PLAY, 16, new Color(255, 255, 255));
        normalPlayIcon = IconFontSwing.buildIcon(FontAwesome.PLAY, 16);

        history = d.history;
        selectedStopIcon = IconFontSwing.buildIcon(FontAwesome.STOP, 16, new Color(255, 255, 255));
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
            final DatenDownload datenDownload = Daten.getInstance().getListeDownloadsButton().getDownloadUrlFilm(datenFilm.arr[DatenFilm.FILM_URL]);

            if (((MVTable) table).lineBreak) {
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
                    if (((MVTable) table).getShowIcons()) {
                        setSenderIcon((String) value, ((MVTable) table).iconKlein);
                    }
                    break;
                case DatenFilm.FILM_HD:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setCheckedOrUncheckedIcon(datenFilm.isHD());
                    setText("");//im Modle brauchen wir den Text zum Sortieren
                    break;
                case DatenFilm.FILM_UT:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setCheckedOrUncheckedIcon(datenFilm.hasSubtitle());
                    setText("");
                    break;
            }

            setColor(this, datenFilm, datenDownload, isSelected);
        } catch (Exception ex) {
            logger.error("Fehler", ex);
        }
        return this;
    }

    private void setColor(Component c, DatenFilm datenFilm, DatenDownload datenDownload, boolean isSelected) {
        final boolean live = datenFilm.getThema().equals(ListeFilme.THEMA_LIVE);
        boolean start = false;

        if (datenDownload != null) {
            // gestarteter Film
            if (datenDownload.start != null) {
                start = true;
                setBackgroundColor(c, datenDownload.start, isSelected);
            }
        }

        if (!start) {
            if (live) {
                // bei livestreams keine History anzeigen
                c.setForeground(MVColor.FILM_LIVESTREAM.color);
            } else if (history.urlPruefen(datenFilm.getUrlHistory())) {
                if (!isSelected) {
                    c.setBackground(MVColor.FILM_HISTORY.color);
                }
            } else if (datenFilm.isNew()) {
                // fix #259
                if (!isSelected)
                    c.setForeground(MVColor.FILM_NEU.color);
            }
        }

        if (geoMelden) {
            if (!start)
                setupGeoblockingBackground(c, datenFilm.arr[DatenFilm.FILM_GEO], isSelected);
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
