package mediathek.tool.cellrenderer;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import mediathek.config.MVColor;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.swing.IconUtils;
import mediathek.tool.SVGIconUtilities;
import mediathek.tool.table.MVTable;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kordamp.ikonli.fontawesome6.FontAwesomeRegular;
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid;
import org.kordamp.ikonli.swing.FontIcon;

import javax.swing.*;
import javax.swing.border.Border;
import java.awt.*;

public class CellRendererDownloads extends CellRendererBaseWithStart {
    private final static String DOWNLOAD_STARTEN = "Download starten";
    private final static String DOWNLOAD_LOESCHEN = "Download aus Liste entfernen";
    private final static String DOWNLOAD_STOPPEN = "Download stoppen";
    private final static String DOWNLOAD_ENTFERNEN = "Download entfernen";
    private final static String PLAY_DOWNLOADED_FILM = "gespeicherten Film abspielen";
    private static final Logger logger = LogManager.getLogger(CellRendererDownloads.class);
    private final FontIcon film_start_tab;
    private final FontIcon film_start_sw_tab;
    private final Border emptyBorder = BorderFactory.createEmptyBorder(3,2,3,2);
    private final Border largeBorder = BorderFactory.createEmptyBorder(9, 2, 9, 2);
    private final JPanel panel;
    private final FontIcon download_stop_tab;
    private final FontIcon download_stop_sw_tab;
    private final FlatSVGIcon download_start_tab;
    private final Icon download_start_sw_tab;
    private final FontIcon download_clear_tab_selected;
    private final FontIcon download_clear_sw_tab;
    private final FontIcon download_del_tab_selected;
    private final FontIcon download_del_sw_tab;
    private final JProgressBar progressBar = new JProgressBar(0, 1000);

    public CellRendererDownloads() {
        download_stop_tab = FontIcon.of(FontAwesomeSolid.STOP, IconUtils.DEFAULT_SIZE, Color.WHITE);
        download_stop_sw_tab = IconUtils.of(FontAwesomeSolid.STOP);

        download_start_tab = SVGIconUtilities.createSVGIcon("icons/fontawesome/caret-down.svg");
        download_start_tab.setColorFilter(new FlatSVGIcon.ColorFilter(_ -> Color.WHITE));
        download_start_sw_tab = SVGIconUtilities.createSVGIcon("icons/fontawesome/caret-down.svg");
        download_clear_tab_selected = FontIcon.of(FontAwesomeSolid.ERASER, IconUtils.DEFAULT_SIZE, Color.WHITE);
        download_clear_sw_tab = IconUtils.of(FontAwesomeSolid.ERASER);

        download_del_tab_selected = FontIcon.of(FontAwesomeRegular.TRASH_ALT, IconUtils.DEFAULT_SIZE, Color.WHITE);
        download_del_sw_tab = IconUtils.of(FontAwesomeRegular.TRASH_ALT);

        film_start_tab = FontIcon.of(FontAwesomeSolid.PLAY, IconUtils.DEFAULT_SIZE, Color.WHITE);
        film_start_sw_tab = IconUtils.of(FontAwesomeSolid.PLAY);

        panel = new JPanel(new BorderLayout());
        panel.add(progressBar);
    }

    private void applyHorizontalAlignment(int colIndex) {
        switch (colIndex) {
            case DatenDownload.DOWNLOAD_PROGRESS, DatenDownload.DOWNLOAD_FILM_NR, DatenDownload.DOWNLOAD_NR, DatenDownload.DOWNLOAD_DATUM, DatenDownload.DOWNLOAD_ZEIT,
                    DatenDownload.DOWNLOAD_DAUER, DatenDownload.DOWNLOAD_BANDBREITE,
                    DatenDownload.DOWNLOAD_RESTZEIT -> setHorizontalAlignment(SwingConstants.CENTER);
            case DatenDownload.DOWNLOAD_GROESSE -> setHorizontalAlignment(SwingConstants.RIGHT);
        }
    }
    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                                                   int row, int column) {
        try {
            resetComponent();
            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

            final int rowModelIndex = table.convertRowIndexToModel(row);
            final int columnModelIndex = table.convertColumnIndexToModel(column);
            DatenDownload datenDownload = (DatenDownload) table.getModel().getValueAt(rowModelIndex, DatenDownload.DOWNLOAD_REF);
            final MVTable mvTable = (MVTable) table;

            if (mvTable.isLineBreak()) {
                setHorizontalAlignment(SwingConstants.LEFT);
                setVerticalAlignment(SwingConstants.TOP);

                switch (columnModelIndex) {
                    case DatenDownload.DOWNLOAD_TITEL, DatenDownload.DOWNLOAD_THEMA, DatenDownload.DOWNLOAD_URL, DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF, DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY, DatenDownload.DOWNLOAD_FILM_URL, DatenDownload.DOWNLOAD_URL_SUBTITLE, DatenDownload.DOWNLOAD_ZIEL_DATEINAME, DatenDownload.DOWNLOAD_ZIEL_PFAD, DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME, DatenDownload.DOWNLOAD_ABO -> {
                        var textArea = new JTextArea();
                        textArea.setLineWrap(true);
                        textArea.setWrapStyleWord(true);
                        textArea.setText(value.toString());
                        textArea.setForeground(getForeground());
                        textArea.setBackground(getBackground());
                        if (columnModelIndex == DatenDownload.DOWNLOAD_ABO) {
                            handleAboColumn(textArea, datenDownload);
                        }
                        setBackgroundColor(textArea, datenDownload.start, isSelected);
                        return textArea;
                    }
                }
            }
            else
                applyHorizontalAlignment(columnModelIndex);

            switch (columnModelIndex) {
                case DatenDownload.DOWNLOAD_PROGRESS -> {
                    if (mvTable.showSenderIcons() && !mvTable.useSmallSenderIcons) {
                        progressBar.setBorder(largeBorder);
                    } else {
                        progressBar.setBorder(emptyBorder);
                    }
                    if (datenDownload.start != null) {
                        if (1 < datenDownload.start.percent && datenDownload.start.percent < Start.PROGRESS_FERTIG) {

                            setBackgroundColor(panel, datenDownload.start, isSelected);
                            setBackgroundColor(progressBar, datenDownload.start, isSelected);

                            progressBar.setValue(datenDownload.start.percent);

                            final double progressValue = datenDownload.start.percent / 10.0;
                            progressBar.setString(Double.toString(progressValue) + '%');

                            return panel;
                        } else {
                            setText(Start.getTextProgress(datenDownload.isDownloadManager(), datenDownload.start));
                        }
                    } else {
                        setText("");
                    }
                }
                case DatenDownload.DOWNLOAD_FILM_NR -> {
                    if ((int) table.getModel().getValueAt(rowModelIndex, DatenDownload.DOWNLOAD_FILM_NR) == 0) {
                        setText("");
                    }
                }
                case DatenDownload.DOWNLOAD_ART -> {
                    switch (datenDownload.art) {
                        case DatenDownload.ART_DOWNLOAD -> setText(DatenDownload.ART_DOWNLOAD_TXT);
                        case DatenDownload.ART_PROGRAMM -> setText(DatenDownload.ART_PROGRAMM_TXT);
                    }
                }
                case DatenDownload.DOWNLOAD_QUELLE -> {
                    switch (datenDownload.quelle) {
                        case DatenDownload.QUELLE_ALLE -> setText(DatenDownload.QUELLE_ALLE_TXT);
                        case DatenDownload.QUELLE_ABO -> setText(DatenDownload.QUELLE_ABO_TXT);
                        case DatenDownload.QUELLE_BUTTON -> setText(DatenDownload.QUELLE_BUTTON_TXT);
                        case DatenDownload.QUELLE_DOWNLOAD -> setText(DatenDownload.QUELLE_DOWNLOAD_TXT);
                    }
                }
                case DatenDownload.DOWNLOAD_BUTTON_START -> handleButtonStartColumn(datenDownload, isSelected);
                case DatenDownload.DOWNLOAD_BUTTON_DEL -> handleButtonDeleteColumn(datenDownload, isSelected);
                case DatenDownload.DOWNLOAD_ABO -> handleAboColumn(datenDownload);
                case DatenDownload.DOWNLOAD_SENDER -> {
                    if (mvTable.showSenderIcons()) {
                        Dimension targetDim = getSenderCellDimension(table, row, columnModelIndex);
                        setSenderIcon(value.toString(), targetDim);
                    }
                }
                case DatenDownload.DOWNLOAD_GEO -> drawGeolocationIcons(datenDownload.film, isSelected);
            }

            if (columnModelIndex == DatenDownload.DOWNLOAD_TITEL) {
                if (datenDownload.film != null) {
                    setIndicatorIcons(table, datenDownload.film, isSelected);
                }
            }

            setBackgroundColor(this, datenDownload.start, isSelected);
        } catch (Exception ex) {
            logger.error(ex);
        }
        return this;
    }

    private void setIconsAndToolTips(DatenDownload datenDownload, Icon filmIcon,
                                     Icon downloadStartIcon, Icon downloadStopIcon) {
        if (datenDownload.start != null && !datenDownload.isDownloadManager()) {
            switch (datenDownload.start.status) {
                case Start.STATUS_FERTIG -> {
                    setIcon(filmIcon);
                    setToolTipText(PLAY_DOWNLOADED_FILM);
                }
                case Start.STATUS_ERR -> {
                    setIcon(downloadStartIcon);
                    setToolTipText(DOWNLOAD_STARTEN);
                }
                default -> {
                    setIcon(downloadStopIcon);
                    setToolTipText(DOWNLOAD_STOPPEN);
                }
            }
        } else {
            setIcon(downloadStartIcon);
            setToolTipText(DOWNLOAD_STARTEN);
        }
    }

    private void handleButtonStartColumn(final DatenDownload datenDownload, final boolean isSelected) {
        setHorizontalAlignment(SwingConstants.CENTER);
        if (isSelected) {
            setIconsAndToolTips(datenDownload, film_start_tab, download_start_tab, download_stop_tab);
        } else {
            setIconsAndToolTips(datenDownload, film_start_sw_tab, download_start_sw_tab, download_stop_sw_tab);
        }
    }

    private void handleAboColumn(JTextArea a, final DatenDownload datenDownload) {
        if (!datenDownload.arr[DatenDownload.DOWNLOAD_ABO].isEmpty()) {
            a.setForeground(MVColor.DOWNLOAD_IST_ABO.color);
        } else {
            a.setForeground(MVColor.DOWNLOAD_IST_DIREKTER_DOWNLOAD.color);
            a.setText("Download");
        }
    }

    private void handleAboColumn(final DatenDownload datenDownload) {
        setHorizontalAlignment(SwingConstants.CENTER);
        if (!datenDownload.arr[DatenDownload.DOWNLOAD_ABO].isEmpty()) {
            setForeground(MVColor.DOWNLOAD_IST_ABO.color);
        } else {
            setForeground(MVColor.DOWNLOAD_IST_DIREKTER_DOWNLOAD.color);
            setText("Download");
        }
    }

    private void handleButtonDeleteColumn(final DatenDownload datenDownload, final boolean isSelected) {
        setHorizontalAlignment(SwingConstants.CENTER);
        if (datenDownload.start != null) {
            if (datenDownload.start.status >= Start.STATUS_FERTIG) {
                setIcons(download_clear_tab_selected, download_clear_sw_tab, DOWNLOAD_ENTFERNEN, isSelected);
            } else {
                setupDownloadLoeschen(isSelected);
            }
        } else {
            setupDownloadLoeschen(isSelected);
        }
    }

    private void setIcons(Icon tab, Icon tab_sw, String text, final boolean isSelected) {
        final Icon icon;
        if (isSelected) {
            icon = tab;
        } else {
            icon = tab_sw;
        }
        setIcon(icon);
        setToolTipText(text);
    }

    private void setupDownloadLoeschen(final boolean isSelected) {
        setIcons(download_del_tab_selected, download_del_sw_tab, DOWNLOAD_LOESCHEN, isSelected);
    }
}
