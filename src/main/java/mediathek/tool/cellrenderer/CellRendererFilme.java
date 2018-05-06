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

import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVColor;
import mediathek.controller.MVUsedUrls;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.tool.MVSenderIconCache;
import mediathek.tool.MVTable;

import javax.swing.*;
import java.awt.*;

@SuppressWarnings("serial")
public class CellRendererFilme extends CellRendererBaseWithStart {
    private static ImageIcon film_rec_tab = null;
    private static ImageIcon film_rec_sw_tab = null;
    private static ImageIcon film_stop_tab = null;
    private static ImageIcon film_stop_sw_tab = null;
    private final MVUsedUrls history;

    public CellRendererFilme(Daten d, MVSenderIconCache cache) {
        super(cache);

        history = d.history;
        film_rec_tab = Icons.ICON_TABELLE_FILM_REC;
        film_rec_sw_tab = Icons.ICON_TABELLE_FILM_REC_SW;
        film_stop_tab = Icons.ICON_TABELLE_FILM_STOP;
        film_stop_sw_tab = Icons.ICON_TABELLE_FILM_STOP_SW;
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
                    case DatenFilm.FILM_BESCHREIBUNG:
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
                    if (((MVTable) table).iconAnzeigen) {
                        handleSenderColumn((String) value, ((MVTable) table).iconKlein);
                    }
                    break;
                case DatenFilm.FILM_NEU:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setYesNoIcon(datenFilm.isNew());
                    setText("");
                    break;
                case DatenFilm.FILM_HD:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setYesNoIcon(datenFilm.isHD());
                    setText("");//im Modle brauchen wir den Text zum Sortieren
                    break;
                case DatenFilm.FILM_UT:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setYesNoIcon(datenFilm.hasSubtitle());
                    setText("");
                    break;
            }
            setColor(this, datenFilm, datenDownload, isSelected);
        } catch (Exception ex) {
            Log.errorLog(630098552, ex);
        }
        return this;
    }

    private void setColor(Component c, DatenFilm datenFilm, DatenDownload datenDownload, boolean isSelected) {
        final boolean live = datenFilm.arr[DatenFilm.FILM_THEMA].equals(ListeFilme.THEMA_LIVE);
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
                        icon = film_stop_tab;
                    else
                        icon = film_stop_sw_tab;

                    setIcon(icon);
                }
            }
        }

        if (getIcon() == null) {
            setToolTipText("Film abspielen");
            final Icon icon;
            if (isSelected)
                icon = film_start_tab;
            else
                icon = film_start_sw_tab;

            setIcon(icon);
        }
    }

    private void handleButtonDownloadColumn(final boolean isSelected) {
        // Button Aufzeichnen
        setHorizontalAlignment(SwingConstants.CENTER);
        setToolTipText("Film aufzeichnen");
        final Icon icon;
        if (isSelected)
            icon = film_rec_tab;
        else
            icon = film_rec_sw_tab;

        setIcon(icon);
    }
}
