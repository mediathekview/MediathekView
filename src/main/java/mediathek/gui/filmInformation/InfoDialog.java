package mediathek.gui.filmInformation;

import com.jidesoft.swing.MultilineLabel;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.MenuItem;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.CornerRadii;
import javafx.scene.paint.Color;
import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVSenderIconCache;
import net.miginfocom.layout.CC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.sync.LockMode;

import javax.swing.*;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.net.URISyntaxException;
import java.util.NoSuchElementException;
import java.util.Optional;

public class InfoDialog extends JDialog {
    private static final long serialVersionUID = -890508930316467747L;
    private static final String FILM_INFO_VISIBLE = "film.information.visible";
    private static final String FILM_INFO_LOCATION_X = "film.information.location.x";
    private static final String FILM_INFO_LOCATION_Y = "film.information.location.y";
    private static final String COPY_URL_TEXT = "URL kopieren";
    private final Configuration config = ApplicationConfiguration.getConfiguration();
    private final MVSenderIconCache senderIconCache;
    private DatenFilm currentFilm = null;
    private JLabel lblSender;
    private MultilineLabel lblThema;
    private MultilineLabel lblTitel;
    private JLabel lblDatum;
    private JLabel lblUhrzeit;
    private JLabel lblDauer;
    private JLabel lblSize;
    private JCheckBox cbHD;
    private JCheckBox cbSubtitle;
    private JLabel lblGeo;
    private JLabel lblAbo;
    private JFXPanel fxPanel;
    private Hyperlink hyperlink;
    private JTextArea lblDescription;

    public InfoDialog(Window parent) {
        super(parent);
        senderIconCache = Daten.getInstance().getSenderIconCache();

        setType(Window.Type.UTILITY);
        setTitle("Filminformation");
        setResizable(false);
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);

        buildLayout();

        installCopyUrlHandler(lblThema);
        installCopyUrlHandler(lblTitel);

        updateTextFields();

        restoreLocation();
        final boolean wasVisible = config.getBoolean(FILM_INFO_VISIBLE, false);
        if (wasVisible) {
            setVisible(true);
        }

        addWindowListener(new WindowAdapter() {
            @Override
            public void windowOpened(WindowEvent e) {
                config.setProperty(FILM_INFO_VISIBLE, true);
            }

            @Override
            public void windowClosed(WindowEvent e) {
                config.setProperty(FILM_INFO_VISIBLE, false);
            }
        });

        //addFilmlistLoadListener();

        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentMoved(ComponentEvent e) {
                if (isVisible())
                    saveLocation();
            }
        });
    }

    private void installCopyUrlHandler(JTextComponent component) {
        JPopupMenu menu = new JPopupMenu();
        JMenuItem item = new JMenuItem(COPY_URL_TEXT);
        item.addActionListener(e -> GuiFunktionen.copyToClipboard(component.getText()));
        menu.add(item);
        component.setComponentPopupMenu(menu);
    }

    /**
     * Restore window position from config settings.
     */
    private void restoreLocation() {
        config.lock(LockMode.READ);
        try {
            final Point newLocation = new Point();
            newLocation.x = config.getInt(FILM_INFO_LOCATION_X);
            newLocation.y = config.getInt(FILM_INFO_LOCATION_Y);
            setLocation(newLocation);
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

        config.lock(LockMode.WRITE);
        try {
            final Point location = getLocationOnScreen();
            config.setProperty(FILM_INFO_LOCATION_X, location.x);
            config.setProperty(FILM_INFO_LOCATION_Y, location.y);
        } finally {
            config.unlock(LockMode.WRITE);
        }
    }

    public void showInfo() {
        updateTextFields();
        pack();

        if (!isVisible())
            setVisible(true);

    }

    private void clearControls() {
        lblSender.setText("");
        lblSender.setIcon(null);
        lblThema.setText("");
        lblTitel.setText("");
        lblDescription.setText("");
        lblSize.setText("");
        lblDatum.setText("");
        lblUhrzeit.setText("");
        lblDauer.setText("");
        cbHD.setSelected(false);
        cbSubtitle.setSelected(false);
        lblGeo.setText("");
        lblAbo.setText("");
        Platform.runLater(() -> {
            hyperlink.setTooltip(null);
            hyperlink.setDisable(true);
        });
    }

    private void setSenderIcon(final JLabel control) {
        final Optional<ImageIcon> optIcon = senderIconCache.get(currentFilm.getSender(), true);
        optIcon.ifPresent(icon -> {
            control.setText("");
            control.setIcon(icon);
        });
    }

    private void updateTextFields() {
        if (currentFilm == null) {
            clearControls();
        } else {
            setSenderIcon(lblSender);

            lblThema.setText(currentFilm.getThema());
            lblTitel.setText(currentFilm.getTitle());

            lblDescription.setText(currentFilm.getDescription());
            lblDescription.setCaretPosition(0);

            lblSize.setText(currentFilm.getSize());
            lblDatum.setText(currentFilm.getSendeDatum());
            lblUhrzeit.setText(currentFilm.getSendeZeit());
            lblDauer.setText(currentFilm.getDauer());

            cbHD.setSelected(currentFilm.isHD());
            cbSubtitle.setSelected(currentFilm.hasSubtitle());

            lblGeo.setText(currentFilm.getGeo().orElse(""));
            lblAbo.setText(currentFilm.getAboName());

            Platform.runLater(() -> {
                hyperlink.setTooltip(new Tooltip(currentFilm.getWebsiteLink()));
                hyperlink.setDisable(false);
            });
        }
    }

    /**
     * This will set the display to the new film data.
     *
     * @param film the film data
     */
    public void updateCurrentFilm(DatenFilm film) {
        currentFilm = film;
        if (isVisible())
            updateTextFields();
        pack();
    }

    private void buildLayout() {
        final Container contentPane = getContentPane();
        contentPane.setLayout(new MigLayout(
                "hidemode 3",
                //columns
                "[fill]" +
                        "[fill]",
                //rows
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
                        "[fill]"));

        JLabel label = new JLabel();
        label.setText("Sender:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, new CC().cell(0,0));

        lblSender = new JLabel();
        contentPane.add(lblSender, new CC().cell(1,0).maxWidth("250"));

        label = new JLabel();
        label.setText("Thema:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label,new CC().cell(0,1));

        lblThema = new MultilineLabel();
        contentPane.add(lblThema, new CC().cell(1,1));

        label = new JLabel();
        label.setText("Titel:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, new CC().cell(0,2));

        lblTitel = new MultilineLabel();
        contentPane.add(lblTitel, new CC().cell(1,2).maxWidth("250"));

        label = new JLabel();
        label.setText("Datum:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, new CC().cell(0,3));

        lblDatum = new JLabel();
        contentPane.add(lblDatum, new CC().cell(1,3).maxWidth("250"));

        label = new JLabel();
        label.setText("Uhrzeit:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, new CC().cell(0,4));

        lblUhrzeit = new JLabel();
        contentPane.add(lblUhrzeit, new CC().cell(1,4).maxWidth("250"));

        label = new JLabel();
        label.setText("Dauer:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, new CC().cell(0,5));

        lblDauer = new JLabel();
        contentPane.add(lblDauer, new CC().cell(1,5).maxWidth("250"));

        label = new JLabel();
        label.setText("Größe (MB):");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, new CC().cell(0,6));

        lblSize = new JLabel();
        contentPane.add(lblSize, new CC().cell(1,6).maxWidth("250"));

        label = new JLabel();
        label.setText("HD:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, new CC().cell(0,7));

        cbHD = new JCheckBox();
        cbHD.setEnabled(false);
        contentPane.add(cbHD, new CC().cell(1,7).maxWidth("250"));

        label = new JLabel();
        label.setText("Untertitel:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, new CC().cell(0,8));

        cbSubtitle = new JCheckBox();
        cbSubtitle.setEnabled(false);
        contentPane.add(cbSubtitle, new CC().cell(1,8).maxWidth("250"));

        label = new JLabel();
        label.setText("Geo:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, new CC().cell(0,9));

        lblGeo = new JLabel();
        contentPane.add(lblGeo, new CC().cell(1,9).maxWidth("250"));

        label = new JLabel();
        label.setText("Abo:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, new CC().cell(0,10));

        lblAbo = new JLabel();
        contentPane.add(lblAbo, new CC().cell(1,10).maxWidth("250"));

        label = new JLabel();
        label.setText("Website:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, new CC().cell(0,11));

        fxPanel = new JFXPanel();
        contentPane.add(fxPanel, new CC().cell(1,11).maxWidth("250"));

        Platform.runLater(() -> {
            ContextMenu contextMenu = new ContextMenu();
            MenuItem mi = new MenuItem(COPY_URL_TEXT);
            mi.setOnAction(e -> SwingUtilities.invokeLater(() -> GuiFunktionen.copyToClipboard(currentFilm.getWebsiteLink())));
            contextMenu.getItems().add(mi);

            hyperlink = new Hyperlink("Hier klicken");
            hyperlink.setContextMenu(contextMenu);
            hyperlink.setUnderline(true);
            hyperlink.setBackground(new Background(new BackgroundFill(Color.rgb(236, 236, 236), CornerRadii.EMPTY, Insets.EMPTY)));
            hyperlink.setOnAction(e -> SwingUtilities.invokeLater(() -> {
                if (currentFilm != null) {
                    try {
                        UrlHyperlinkAction.openURL(null, currentFilm.getWebsiteLink());
                    } catch (URISyntaxException ex) {
                        ex.printStackTrace();
                    }
                }
            }));
            fxPanel.setScene(new Scene(hyperlink));
        });
        label = new JLabel();
        label.setText("Beschreibung:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, new CC().cell(0,12));

        final JScrollPane scrollPane = new JScrollPane();
        lblDescription = new JTextArea();
        lblDescription.setLineWrap(true);
        lblDescription.setEditable(false);
        lblDescription.setWrapStyleWord(true);
        lblDescription.setFont(UIManager.getFont("Label.font"));
        scrollPane.setViewportView(lblDescription);

        contentPane.add(scrollPane, "cell 0 13 2 1,growy,width 250:320:400,height 100:100");
        pack();
    }
}
