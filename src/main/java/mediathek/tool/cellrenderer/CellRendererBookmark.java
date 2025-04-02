package mediathek.tool.cellrenderer;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import mediathek.config.Daten;
import mediathek.config.MVColor;
import mediathek.controller.history.SeenHistoryController;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.bookmark.DatenBookmark;
import mediathek.tool.ColorUtils;
import mediathek.tool.SVGIconUtilities;
import mediathek.tool.table.MVTable;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;

public class CellRendererBookmark extends CellRendererBaseWithStart {
  private static final Logger logger = LogManager.getLogger(CellRendererBookmark.class);
  private final SeenHistoryController history = new SeenHistoryController();
  private final FlatSVGIcon selectedDownloadIcon;
  private final FlatSVGIcon normalDownloadIcon;
  private final FlatSVGIcon selectedPlayIcon;
  private final FlatSVGIcon normalPlayIcon;

  public CellRendererBookmark() {
    selectedDownloadIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/download.svg");
    selectedDownloadIcon.setColorFilter(whiteColorFilter);

    normalDownloadIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/download.svg");

    selectedPlayIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/play.svg");
    selectedPlayIcon.setColorFilter(whiteColorFilter);

    normalPlayIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/play.svg");
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
      final DatenBookmark datenFilm = (DatenBookmark) table.getModel().getValueAt(rowModelIndex, DatenFilm.FILM_REF);
      final DatenDownload datenDownload = Daten.getInstance().getListeDownloadsButton().getDownloadUrlFilm(datenFilm.getUrl());
      final var mvTable = (MVTable) table;


      //shortcut if we want to have line breaks, use text areas and skip the rest
      if (mvTable.isLineBreak()) {
        setHorizontalAlignment(SwingConstants.LEFT);
        setVerticalAlignment(SwingConstants.TOP);

        switch (columnModelIndex) {
          case DatenBookmark.BOOKMARK_THEMA , DatenBookmark.BOOKMARK_TITEL , DatenBookmark.BOOKMARK_URL -> {
            var textArea = createTextArea(value.toString());
            applyColorSettings(textArea, datenFilm, isSelected);
            return textArea;
          }
        }
      }
      else
        applyHorizontalAlignment(columnModelIndex);

      //NOT IN LINEBREAK MODE -> REGULAR HANDLING
      //here comes the content...
      switch (columnModelIndex) {
        case DatenBookmark.BOOKMARK_DAUER -> setText(datenFilm.getFilmLengthAsString());
        case DatenBookmark.BOOKMARK_ABSPIELEN -> handleButtonStartColumn(datenDownload, isSelected);
        case DatenBookmark.BOOKMARK_AUFZEICHNEN -> handleButtonDownloadColumn(isSelected);

        case DatenBookmark.BOOKMARK_SENDER -> {
          if (mvTable.showSenderIcons()) {
            Dimension targetDim = getSenderCellDimension(table, row, columnModelIndex);
            setSenderIcon(value.toString(), targetDim);
          }
        }
        case DatenBookmark.BOOKMARK_TITEL -> {
          setText(datenFilm.getTitel());
          setIndicatorIcons(table, datenFilm.getDataAsDatenFilm(), isSelected);
        }
        }

      applyColorSettings(this, datenFilm, isSelected);
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
      case DatenBookmark.BOOKMARK_DATUM, DatenBookmark.BOOKMARK_DAUER, DatenBookmark.BOOKMARK_ABSPIELEN, DatenBookmark.BOOKMARK_AUFZEICHNEN ->
          setHorizontalAlignment(SwingConstants.CENTER);
      }
  }

  private final java.util.List<Color> bgList = new ArrayList<>();

  private void applyColorSettings(Component c, @NotNull DatenBookmark datenFilm, boolean isSelected) {
    bgList.clear();

    bgList.add(c.getBackground());


    if (bgList.size() >= 2)
      c.setBackground(ColorUtils.blend(bgList.toArray(new Color[0])));
    else
      c.setBackground(bgList.getFirst());
  }

  private void handleButtonStartColumn(final DatenDownload datenDownload, final boolean isSelected) {
    // Button Abspielen
    if (datenDownload != null) {
      if (datenDownload.start != null) {
        if (datenDownload.start.status == Start.STATUS_RUN) {

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

}
