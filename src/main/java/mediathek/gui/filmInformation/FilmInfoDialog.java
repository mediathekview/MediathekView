/*
 * Copyright (c) 2024-2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.filmInformation;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import com.formdev.flatlaf.util.ScaledImageIcon;
import mediathek.config.Konstanten;
import mediathek.daten.DatenFilm;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.CopyToClipboardAction;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.SwingErrorDialog;
import mediathek.tool.datum.DateUtil;
import mediathek.tool.sender_icon_cache.MVSenderIconCache;
import mediathek.tool.sender_icon_cache.SenderIconRenderUtil;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.configuration2.sync.LockMode;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.net.URI;
import java.util.NoSuchElementException;
import java.util.Optional;

/**
 * @author christianfranzke
 */
public class FilmInfoDialog extends JDialog {
    private static final Dimension DEFAULT_SENDER_DIMENSION = new Dimension(48, 48);
    private static final Dimension DEFAULT_SENDER_HEIGHT_BOUNDARY = new Dimension(4096, DEFAULT_SENDER_DIMENSION.height);
    private final JPopupMenu popupMenu = new JPopupMenu();
    private Optional<DatenFilm> currentFilmOptional = Optional.empty();
    private FilmAvailableUntilWorker currentWorker;

    public FilmInfoDialog(Window owner) {
        super(owner);
        initComponents();

        setupDescriptionPopupMenu();

        setupHyperlink();

        updateTextFields();
        restoreLocation();

        setVisible(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.FilmInfoDialog.VISIBLE, false));
        setupListeners();
    }

    private void setupDescriptionPopupMenu() {
        var item = new JMenuItem("Auswahl kopieren");
        item.addActionListener(_ -> {
            final var selected = (lblDescription.getSelectionEnd() - lblDescription.getSelectionStart()) > 0;
            if (!selected) {
                JOptionPane.showMessageDialog(this, "Kein Text markiert!", Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
            }
            else {
                GuiFunktionen.copyToClipboard(lblDescription.getSelectedText());
            }
        });
        popupMenu.add(item);
        lblDescription.setComponentPopupMenu(popupMenu);
    }

    private void setupListeners() {
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowOpened(WindowEvent e) {
                ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.FilmInfoDialog.VISIBLE, true);
            }

            @Override
            public void windowClosed(WindowEvent e) {
                ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.FilmInfoDialog.VISIBLE, false);
            }
        });
        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                saveLocation();
            }

            @Override
            public void componentMoved(ComponentEvent e) {
                if (isVisible()) {
                    saveLocation();
                }
            }
        });
    }

    /**
     * Restore window position from config settings.
     */
    private void restoreLocation() {
        var config = ApplicationConfiguration.getConfiguration();
        config.lock(LockMode.READ);
        try {
            var newLocation = new Point(config.getInt(ApplicationConfiguration.FilmInfoDialog.X),
                    config.getInt(ApplicationConfiguration.FilmInfoDialog.Y));
            setLocation(newLocation);

            var w = config.getInt(ApplicationConfiguration.FilmInfoDialog.WIDTH);
            var h = config.getInt(ApplicationConfiguration.FilmInfoDialog.HEIGHT);
            if (w > 50 && h > 50) {
                setSize(new Dimension(w, h));
            }
        } catch (NoSuchElementException ignored) {
        } finally {
            config.unlock(LockMode.READ);
        }
    }

    /**
     * Save window position to config.
     */
    private void saveLocation() {
        //prevent strange OpenJDK bug on Linux where getLocationOnScreen will fail if not visible...
        if (!isVisible())
            return;
        var config = ApplicationConfiguration.getConfiguration();
        config.lock(LockMode.WRITE);
        try {
            var location = getLocationOnScreen();
            config.setProperty(ApplicationConfiguration.FilmInfoDialog.X, location.x);
            config.setProperty(ApplicationConfiguration.FilmInfoDialog.Y, location.y);
            config.setProperty(ApplicationConfiguration.FilmInfoDialog.WIDTH, getWidth());
            config.setProperty(ApplicationConfiguration.FilmInfoDialog.HEIGHT, getHeight());
        } finally {
            config.unlock(LockMode.WRITE);
        }
    }

    private void setupHyperlink() {
        hyperlink.addActionListener(_ -> {
            if (!hyperlink.getToolTipText().isEmpty()) {
                var toolTipText = hyperlink.getToolTipText();
                if (Desktop.isDesktopSupported()) {
                    var d = Desktop.getDesktop();
                    if (d.isSupported(Desktop.Action.BROWSE)) {
                        try {
                            d.browse(new URI(toolTipText));
                        } catch (Exception ex) {
                            SwingErrorDialog.showExceptionMessage(
                                    MediathekGui.ui(),
                                    "Es trat ein Fehler beim Öffnen des Links auf.\nSollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                                    ex);
                        }
                    } else {
                        UrlHyperlinkAction.openURL(toolTipText);
                    }
                } else {
                    UrlHyperlinkAction.openURL(toolTipText);
                }
            }
        });
    }

    public void showInfo() {
        updateTextFields();

        if (!isVisible())
            setVisible(true);
    }

    public void updateCurrentFilm(DatenFilm film) {
        if (film == null)
            currentFilmOptional = Optional.empty();
        else
            currentFilmOptional = Optional.of(film);

        if (isVisible()) {
            updateTextFields();
        }
    }

    private void clearControls() {
        lblSender.setText("");
        lblThema.setText("");
        lblTitel.setText("");
        lblDate.setText("");
        lblUhrzeit.setText("");
        lblDuration.setText("");
        lblSize.setText("");
        lblGeo.setText("");
        cbHq.setSelected(false);
        cbSubtitle.setSelected(false);
        lblAbo.setText("");
        hyperlink.setToolTipText("");
        hyperlink.setEnabled(false);
        hyperlink.setComponentPopupMenu(null);
        lblDescription.setText("");
        lblSeason.setText("");
        lblEpisode.setText("");
        lblAvailableUntil.setText("");
    }

    private void updateTextFields() {
        currentFilmOptional.ifPresentOrElse(currentFilm -> {
            MVSenderIconCache.get(currentFilm.getSender()).ifPresentOrElse(icon -> {
                lblSender.setText("");
                Icon renderedIcon;
                if (icon instanceof FlatSVGIcon svg) {
                    renderedIcon = createSvgIconCroppedToHeight(svg, DEFAULT_SENDER_DIMENSION.height);
                } else {
                    var imageDim = new Dimension(icon.getIconWidth(), icon.getIconHeight());
                    var destDim = SenderIconRenderUtil.calculateFittedDimensionAllowUpscale(imageDim, DEFAULT_SENDER_HEIGHT_BOUNDARY);
                    renderedIcon = new ScaledImageIcon(icon, destDim.width, destDim.height);
                }
                lblSender.setIcon(renderedIcon);
            }, () -> {
                lblSender.setText(currentFilm.getSender());
                lblSender.setIcon(null);
            });

            lblThema.setText(currentFilm.getThema());
            lblTitel.setText(currentFilm.getTitle());
            lblDate.setText(currentFilm.getSendeDatum());
            lblUhrzeit.setText(currentFilm.getSendeZeit());
            lblDuration.setText(currentFilm.getFilmLengthAsString());
            lblSize.setText(currentFilm.getFileSize().toString());
            if (!currentFilm.hasCountries()) {
                lblGeo.setText("");
            }
            else {
                lblGeo.setText(currentFilm.getCountriesAsString());
            }
            cbHq.setSelected(currentFilm.isHighQuality());
            cbSubtitle.setSelected(currentFilm.hasSubtitle());

            currentFilm.getAboOptional().ifPresentOrElse(abo -> lblAbo.setText(abo.getName()), () -> lblAbo.setText(""));

            prepareHyperlink(currentFilm.getWebsiteUrl());

            lblDescription.setText(currentFilm.getDescription().trim());
            SwingUtilities.invokeLater(() -> descScrollPane.getVerticalScrollBar().setValue(0));

            if (currentFilm.getSeason() != 0) {
                lblSeason.setText(String.valueOf(currentFilm.getSeason()));
            }
            else {
                lblSeason.setText("");
            }
            if (currentFilm.getEpisode() != 0) {
                lblEpisode.setText(String.valueOf(currentFilm.getEpisode()));
            }
            else {
                lblEpisode.setText("");
            }

            if (currentWorker != null && !currentWorker.isDone()) {
                currentWorker.cancel(true);
            }
            var availableUntil = currentFilm.getAvailableUntil();
            if (availableUntil == null) {
                currentWorker = new FilmAvailableUntilWorker(currentFilm, lblAvailableUntil);
                currentWorker.execute();
            }
            else {
                lblAvailableUntil.setText(DateUtil.FORMATTER.format(availableUntil));
            }
        }, this::clearControls);
    }

    private static Icon createSvgIconCroppedToHeight(FlatSVGIcon svg, int targetHeight) {
        int baseWidth = Math.max(1, svg.getIconWidth());
        int baseHeight = Math.max(1, svg.getIconHeight());
        int outHeight = Math.max(1, targetHeight);

        // Keep probe image bounded by target height to avoid large temporary allocations
        // for oversized source SVG canvases (e.g. mm-based exports).
        final float scaleToTarget = outHeight / (float) baseHeight;
        final float probeScale = Math.max(scaleToTarget * 4.0f, 0.05f);
        FlatSVGIcon probe = svg.derive(probeScale);
        BufferedImage probeImage = new BufferedImage(
                Math.max(1, probe.getIconWidth()),
                Math.max(1, probe.getIconHeight()),
                BufferedImage.TYPE_INT_ARGB
        );
        Graphics2D pg = probeImage.createGraphics();
        try {
            probe.paintIcon(null, pg, 0, 0);
        } finally {
            pg.dispose();
        }

        Rectangle bounds = opaqueBounds(probeImage);
        if (bounds == null) {
            var fallbackDim = SenderIconRenderUtil.calculateFittedDimensionAllowUpscale(
                    new Dimension(baseWidth, baseHeight),
                    new Dimension(DEFAULT_SENDER_HEIGHT_BOUNDARY.width, outHeight)
            );
            return svg.derive(fallbackDim.width, fallbackDim.height);
        }

        double invScale = 1.0d / probeScale;
        double cropX = bounds.x * invScale;
        double cropY = bounds.y * invScale;
        double cropW = Math.max(1.0d, bounds.width * invScale);
        double cropH = Math.max(1.0d, bounds.height * invScale);

        double scale = outHeight / cropH;
        int derivedWidth = Math.max(1, (int) Math.round(baseWidth * scale));
        int derivedHeight = Math.max(1, (int) Math.round(baseHeight * scale));
        FlatSVGIcon derived = svg.derive(derivedWidth, derivedHeight);

        int offsetX = Math.max(0, (int) Math.round(cropX * scale));
        int offsetY = Math.max(0, (int) Math.round(cropY * scale));
        int outWidth = Math.max(1, (int) Math.round(cropW * scale));

        return new CroppedDelegateIcon(derived, offsetX, offsetY, outWidth, outHeight);
    }

    private static Rectangle opaqueBounds(BufferedImage image) {
        int minX = image.getWidth();
        int minY = image.getHeight();
        int maxX = -1;
        int maxY = -1;

        for (int y = 0; y < image.getHeight(); y++) {
            for (int x = 0; x < image.getWidth(); x++) {
                int alpha = (image.getRGB(x, y) >>> 24) & 0xFF;
                if (alpha != 0) {
                    if (x < minX) minX = x;
                    if (y < minY) minY = y;
                    if (x > maxX) maxX = x;
                    if (y > maxY) maxY = y;
                }
            }
        }

        if (maxX < minX || maxY < minY) {
            return null;
        }
        return new Rectangle(minX, minY, (maxX - minX) + 1, (maxY - minY) + 1);
    }

    private static final class CroppedDelegateIcon implements Icon {
        private final Icon delegate;
        private final int offsetX;
        private final int offsetY;
        private final int width;
        private final int height;

        private CroppedDelegateIcon(Icon delegate, int offsetX, int offsetY, int width, int height) {
            this.delegate = delegate;
            this.offsetX = offsetX;
            this.offsetY = offsetY;
            this.width = width;
            this.height = height;
        }

        @Override
        public void paintIcon(Component c, Graphics g, int x, int y) {
            delegate.paintIcon(c, g, x - offsetX, y - offsetY);
        }

        @Override
        public int getIconWidth() {
            return width;
        }

        @Override
        public int getIconHeight() {
            return height;
        }
    }

    private void prepareHyperlink(String url) {
        hyperlink.setEnabled(true);
        hyperlink.setToolTipText(url);
        hyperlink.setClicked(false);
        var urlPopupMenu = new JPopupMenu();
        urlPopupMenu.add(new CopyToClipboardAction(url));
        hyperlink.setComponentPopupMenu(urlPopupMenu);
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents  @formatter:off
        // Generated using JFormDesigner non-commercial license
        var label1 = new JLabel();
        lblSender = new JLabel();
        var label2 = new JLabel();
        lblThema = new HtmlMultilineLabel();
        var label3 = new JLabel();
        lblTitel = new HtmlMultilineLabel();
        var label4 = new JLabel();
        lblDate = new JLabel();
        var label5 = new JLabel();
        lblUhrzeit = new JLabel();
        var label6 = new JLabel();
        lblDuration = new JLabel();
        var label7 = new JLabel();
        lblSize = new JLabel();
        var label8 = new JLabel();
        cbHq = new DisabledCheckBox();
        var label9 = new JLabel();
        cbSubtitle = new DisabledCheckBox();
        var label12 = new JLabel();
        lblSeason = new JLabel();
        var label14 = new JLabel();
        lblEpisode = new JLabel();
        var label15 = new JLabel();
        lblAvailableUntil = new JLabel();
        var label10 = new JLabel();
        lblGeo = new JLabel();
        var label11 = new JLabel();
        lblAbo = new JLabel();
        hyperlink = new JXHyperlink();
        var label13 = new JLabel();
        descScrollPane = new JScrollPane();
        lblDescription = new JTextPane();

        //======== this ========
        setType(Window.Type.UTILITY);
        setTitle("Filminformation");
        setMaximumSize(new Dimension(500, 800));
        setMinimumSize(new Dimension(320, 240));
        setPreferredSize(new Dimension(400, 500));
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        var contentPane = getContentPane();
        contentPane.setLayout(new MigLayout(
            "insets 5,hidemode 3",
            // columns
            "[fill]" +
            "[grow,fill]",
            // rows
            "[]" +
            "[]" +
            "[]" +
            "[]" +
            "[]" +
            "[]" +
            "[]" +
            "[]" +
            "[]" +
            "[]" +
            "[]" +
            "[]" +
            "[]" +
            "[]" +
            "[]" +
            "[]" +
            "[grow,fill]"));

        //---- label1 ----
        label1.setText("Sender:");
        contentPane.add(label1, "cell 0 0");
        contentPane.add(lblSender, "cell 1 0");

        //---- label2 ----
        label2.setText("Thema:");
        contentPane.add(label2, "cell 0 1");

        //---- lblThema ----
        lblThema.setPreferredSize(new Dimension(443, 32));
        lblThema.setMinimumSize(new Dimension(51, 32));
        contentPane.add(lblThema, "cell 1 1");

        //---- label3 ----
        label3.setText("Titel:");
        contentPane.add(label3, "cell 0 2");
        contentPane.add(lblTitel, "cell 1 2");

        //---- label4 ----
        label4.setText("Datum:");
        contentPane.add(label4, "cell 0 3");

        //---- lblDate ----
        lblDate.setText("text");
        contentPane.add(lblDate, "cell 1 3");

        //---- label5 ----
        label5.setText("Uhrzeit:");
        contentPane.add(label5, "cell 0 4");

        //---- lblUhrzeit ----
        lblUhrzeit.setText("text");
        contentPane.add(lblUhrzeit, "cell 1 4");

        //---- label6 ----
        label6.setText("Dauer:");
        contentPane.add(label6, "cell 0 5");

        //---- lblDuration ----
        lblDuration.setText("text");
        contentPane.add(lblDuration, "cell 1 5");

        //---- label7 ----
        label7.setText("Gr\u00f6\u00dfe (MB):");
        contentPane.add(label7, "cell 0 6");

        //---- lblSize ----
        lblSize.setText("text");
        contentPane.add(lblSize, "cell 1 6");

        //---- label8 ----
        label8.setText("HQ:");
        contentPane.add(label8, "cell 0 7");
        contentPane.add(cbHq, "cell 1 7");

        //---- label9 ----
        label9.setText("Untertitel:");
        contentPane.add(label9, "cell 0 8");
        contentPane.add(cbSubtitle, "cell 1 8");

        //---- label12 ----
        label12.setText("Season:");
        contentPane.add(label12, "cell 0 9");

        //---- lblSeason ----
        lblSeason.setText("text");
        contentPane.add(lblSeason, "cell 1 9,growx");

        //---- label14 ----
        label14.setText("Episode:");
        contentPane.add(label14, "cell 0 10");

        //---- lblEpisode ----
        lblEpisode.setText("text");
        contentPane.add(lblEpisode, "cell 1 10,growx");

        //---- label15 ----
        label15.setText("Verf\u00fcgbar bis:");
        contentPane.add(label15, "cell 0 11");

        //---- lblAvailableUntil ----
        lblAvailableUntil.setText("text");
        contentPane.add(lblAvailableUntil, "cell 1 11");

        //---- label10 ----
        label10.setText("Geo:");
        contentPane.add(label10, "cell 0 12");

        //---- lblGeo ----
        lblGeo.setText("text");
        contentPane.add(lblGeo, "cell 1 12");

        //---- label11 ----
        label11.setText("Abo:");
        contentPane.add(label11, "cell 0 13");

        //---- lblAbo ----
        lblAbo.setText("text");
        contentPane.add(lblAbo, "cell 1 13");

        //---- hyperlink ----
        hyperlink.setText("Link zur Webseite");
        contentPane.add(hyperlink, "cell 0 14 2 1");

        //---- label13 ----
        label13.setText("Beschreibung:");
        contentPane.add(label13, "cell 0 15 2 1");

        //======== descScrollPane ========
        {

            //---- lblDescription ----
            lblDescription.setMinimumSize(new Dimension(1, 100));
            lblDescription.setPreferredSize(new Dimension(1, 100));
            lblDescription.setMaximumSize(new Dimension(2147483647, 200));
            descScrollPane.setViewportView(lblDescription);
        }
        contentPane.add(descScrollPane, "cell 0 16 2 1");
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    private JLabel lblSender;
    private HtmlMultilineLabel lblThema;
    private HtmlMultilineLabel lblTitel;
    private JLabel lblDate;
    private JLabel lblUhrzeit;
    private JLabel lblDuration;
    private JLabel lblSize;
    private DisabledCheckBox cbHq;
    private DisabledCheckBox cbSubtitle;
    private JLabel lblSeason;
    private JLabel lblEpisode;
    private JLabel lblAvailableUntil;
    private JLabel lblGeo;
    private JLabel lblAbo;
    private JXHyperlink hyperlink;
    private JScrollPane descScrollPane;
    private JTextPane lblDescription;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
