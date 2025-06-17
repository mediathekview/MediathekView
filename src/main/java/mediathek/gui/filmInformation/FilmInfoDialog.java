/*
 * Created by JFormDesigner on Sun Apr 28 11:14:19 CEST 2024
 */

package mediathek.gui.filmInformation;

import com.formdev.flatlaf.util.ScaledImageIcon;
import mediathek.config.Konstanten;
import mediathek.daten.Country;
import mediathek.daten.DatenFilm;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.CopyToClipboardAction;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.SwingErrorDialog;
import mediathek.tool.sender_icon_cache.MVSenderIconCache;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.configuration2.sync.LockMode;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.net.URI;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * @author christianfranzke
 */
public class FilmInfoDialog extends JDialog {
    private static final Dimension DEFAULT_SENDER_DIMENSION = new Dimension(64, 64);
    private Optional<DatenFilm> currentFilmOptional = Optional.empty();
    private final JPopupMenu popupMenu = new JPopupMenu();

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
    }

    private void updateTextFields() {
        currentFilmOptional.ifPresentOrElse(currentFilm -> {
            MVSenderIconCache.get(currentFilm.getSender()).ifPresentOrElse(icon -> {
                lblSender.setText("");
                var imageDim = new Dimension(icon.getIconWidth(), icon.getIconHeight());
                var destDim = GuiFunktionen.calculateFittedDimension(imageDim, DEFAULT_SENDER_DIMENSION);
                lblSender.setIcon(new ScaledImageIcon(icon, destDim.width, destDim.height));
            }, () -> lblSender.setText(currentFilm.getSender()));

            lblThema.setText(currentFilm.getThema());
            lblTitel.setText(currentFilm.getTitle());
            lblDate.setText(currentFilm.getSendeDatum());
            lblUhrzeit.setText(currentFilm.getSendeZeit());
            lblDuration.setText(currentFilm.getFilmLengthAsString());
            lblSize.setText(currentFilm.getFileSize().toString());
            if (currentFilm.countrySet.isEmpty()) {
                lblGeo.setText("");
            } else {
                var txt = currentFilm.countrySet.stream().map(Country::toString).collect(Collectors.joining("-"));
                lblGeo.setText(txt);
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
            else  {
                lblSeason.setText("");
            }
            if (currentFilm.getEpisode() != 0) {
                lblEpisode.setText(String.valueOf(currentFilm.getEpisode()));
            }
            else  {
                lblEpisode.setText("");
            }
        }, this::clearControls);
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
        var label12 = new JLabel();
        lblSeason = new JLabel();
        var label14 = new JLabel();
        lblEpisode = new JLabel();
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
            new LC().insets("5").hideMode(3),
            // columns
            new AC()
                .fill().gap()
                .grow().fill(),
            // rows
            new AC()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .gap()
                .grow().fill()));

        //---- label1 ----
        label1.setText("Sender:");
        contentPane.add(label1, new CC().cell(0, 0));
        contentPane.add(lblSender, new CC().cell(1, 0));

        //---- label2 ----
        label2.setText("Thema:");
        contentPane.add(label2, new CC().cell(0, 1));

        //---- lblThema ----
        lblThema.setPreferredSize(new Dimension(443, 32));
        lblThema.setMinimumSize(new Dimension(51, 32));
        contentPane.add(lblThema, new CC().cell(1, 1));

        //---- label12 ----
        label12.setText("Season:");
        contentPane.add(label12, new CC().cell(0, 2));

        //---- lblSeason ----
        lblSeason.setText("text");
        contentPane.add(lblSeason, new CC().cell(1, 2).growX());

        //---- label14 ----
        label14.setText("Episode:");
        contentPane.add(label14, new CC().cell(0, 3));

        //---- lblEpisode ----
        lblEpisode.setText("text");
        contentPane.add(lblEpisode, new CC().cell(1, 3).growX());

        //---- label3 ----
        label3.setText("Titel:");
        contentPane.add(label3, new CC().cell(0, 4));
        contentPane.add(lblTitel, new CC().cell(1, 4));

        //---- label4 ----
        label4.setText("Datum:");
        contentPane.add(label4, new CC().cell(0, 5));

        //---- lblDate ----
        lblDate.setText("text");
        contentPane.add(lblDate, new CC().cell(1, 5));

        //---- label5 ----
        label5.setText("Uhrzeit:");
        contentPane.add(label5, new CC().cell(0, 6));

        //---- lblUhrzeit ----
        lblUhrzeit.setText("text");
        contentPane.add(lblUhrzeit, new CC().cell(1, 6));

        //---- label6 ----
        label6.setText("Dauer:");
        contentPane.add(label6, new CC().cell(0, 7));

        //---- lblDuration ----
        lblDuration.setText("text");
        contentPane.add(lblDuration, new CC().cell(1, 7));

        //---- label7 ----
        label7.setText("Gr\u00f6\u00dfe (MB):");
        contentPane.add(label7, new CC().cell(0, 8));

        //---- lblSize ----
        lblSize.setText("text");
        contentPane.add(lblSize, new CC().cell(1, 8));

        //---- label8 ----
        label8.setText("HQ:");
        contentPane.add(label8, new CC().cell(0, 9));
        contentPane.add(cbHq, new CC().cell(1, 9));

        //---- label9 ----
        label9.setText("Untertitel:");
        contentPane.add(label9, new CC().cell(0, 10));
        contentPane.add(cbSubtitle, new CC().cell(1, 10));

        //---- label10 ----
        label10.setText("Geo:");
        contentPane.add(label10, new CC().cell(0, 11));

        //---- lblGeo ----
        lblGeo.setText("text");
        contentPane.add(lblGeo, new CC().cell(1, 11));

        //---- label11 ----
        label11.setText("Abo:");
        contentPane.add(label11, new CC().cell(0, 12));

        //---- lblAbo ----
        lblAbo.setText("text");
        contentPane.add(lblAbo, new CC().cell(1, 12));

        //---- hyperlink ----
        hyperlink.setText("Link zur Website");
        contentPane.add(hyperlink, new CC().cell(0, 13, 2, 1));

        //---- label13 ----
        label13.setText("Beschreibung:");
        contentPane.add(label13, new CC().cell(0, 14, 2, 1));

        //======== descScrollPane ========
        {

            //---- lblDescription ----
            lblDescription.setMinimumSize(new Dimension(1, 100));
            lblDescription.setPreferredSize(new Dimension(1, 100));
            lblDescription.setMaximumSize(new Dimension(2147483647, 200));
            descScrollPane.setViewportView(lblDescription);
        }
        contentPane.add(descScrollPane, new CC().cell(0, 15, 2, 1));
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents  @formatter:on
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables  @formatter:off
    // Generated using JFormDesigner non-commercial license
    private JLabel lblSender;
    private HtmlMultilineLabel lblThema;
    private JLabel lblSeason;
    private JLabel lblEpisode;
    private HtmlMultilineLabel lblTitel;
    private JLabel lblDate;
    private JLabel lblUhrzeit;
    private JLabel lblDuration;
    private JLabel lblSize;
    private DisabledCheckBox cbHq;
    private DisabledCheckBox cbSubtitle;
    private JLabel lblGeo;
    private JLabel lblAbo;
    private JXHyperlink hyperlink;
    private JScrollPane descScrollPane;
    private JTextPane lblDescription;
    // JFormDesigner - End of variables declaration  //GEN-END:variables  @formatter:on
}
