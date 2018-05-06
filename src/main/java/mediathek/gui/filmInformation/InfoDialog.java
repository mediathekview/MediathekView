package mediathek.gui.filmInformation;

import com.jidesoft.swing.MultilineLabel;
import mSearch.daten.DatenFilm;
import mSearch.tool.ApplicationConfiguration;
import mediathek.gui.HyperlinkButton;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.tool.MVSenderIconCache;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.sync.LockMode;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.net.URISyntaxException;
import java.util.NoSuchElementException;

public class InfoDialog extends JDialog {
    private static final long serialVersionUID = -890508930316467747L;
    private static final String FILM_INFO_VISIBLE = "film.information.visible";
    private static final String FILM_INFO_LOCATION_X = "film.information.location.x";
    private static final String FILM_INFO_LOCATION_Y = "film.information.location.y";
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
    private HyperlinkButton btnLinkWebsite;
    private JTextArea lblDescription;

    public InfoDialog(Window parent, MVSenderIconCache cache) {
        super(parent);
        senderIconCache = cache;

        setType(Window.Type.UTILITY);
        setTitle("Filminformation");
        setResizable(false);
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);

        buildLayout();

        btnLinkWebsite.addActionListener((e) -> {
            if (currentFilm != null) {
                try {
                    UrlHyperlinkAction.openURL(null, currentFilm.getWebsiteLink());
                } catch (URISyntaxException ex) {
                    ex.printStackTrace();
                }
            }
        });
        updateTextFields();

        setLocationRelativeTo(getOwner());

        final boolean wasVisible = config.getBoolean(FILM_INFO_VISIBLE, false);
        if (wasVisible) {
            restoreLocation();
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

        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentMoved(ComponentEvent e) {
                saveLocation();
            }
        });
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
        btnLinkWebsite.setToolTipText("");
        btnLinkWebsite.setEnabled(false);
    }

    private void setSenderIcon(final JLabel control) {
        final ImageIcon icon = senderIconCache.get(currentFilm.getSender(), true);
        if (icon != null) {
            control.setText("");
            control.setIcon(icon);
        }
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
            lblDatum.setText(currentFilm.arr[DatenFilm.FILM_DATUM]);
            lblUhrzeit.setText(currentFilm.arr[DatenFilm.FILM_ZEIT]);
            lblDauer.setText(currentFilm.arr[DatenFilm.FILM_DAUER]);

            cbHD.setSelected(currentFilm.isHD());
            cbSubtitle.setSelected(currentFilm.hasSubtitle());

            lblGeo.setText(currentFilm.arr[DatenFilm.FILM_GEO]);
            lblAbo.setText(currentFilm.arr[DatenFilm.FILM_ABO_NAME]);

            btnLinkWebsite.setToolTipText(currentFilm.getWebsiteLink());
            btnLinkWebsite.setEnabled(true);
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
        contentPane.add(label, "cell 0 0");

        lblSender = new JLabel();
        contentPane.add(lblSender, "cell 1 0,wmax 250");

        label = new JLabel();
        label.setText("Thema:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, "cell 0 1");

        lblThema = new MultilineLabel();
        contentPane.add(lblThema, "cell 1 1");

        label = new JLabel();
        label.setText("Titel:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, "cell 0 2");

        lblTitel = new MultilineLabel();
        contentPane.add(lblTitel, "cell 1 2,wmax 250");

        label = new JLabel();
        label.setText("Datum:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, "cell 0 3");

        lblDatum = new JLabel();
        contentPane.add(lblDatum, "cell 1 3,wmax 250");

        label = new JLabel();
        label.setText("Uhrzeit:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, "cell 0 4");

        lblUhrzeit = new JLabel();
        contentPane.add(lblUhrzeit, "cell 1 4,wmax 250");

        label = new JLabel();
        label.setText("Dauer:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, "cell 0 5");

        lblDauer = new JLabel();
        contentPane.add(lblDauer, "cell 1 5,wmax 250");

        label = new JLabel();
        label.setText("Größe (MB):");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, "cell 0 6");

        lblSize = new JLabel();
        contentPane.add(lblSize, "cell 1 6,wmax 250");

        label = new JLabel();
        label.setText("HD:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, "cell 0 7");

        cbHD = new JCheckBox();
        cbHD.setEnabled(false);
        contentPane.add(cbHD, "cell 1 7,wmax 250");

        label = new JLabel();
        label.setText("Untertitel:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, "cell 0 8");

        cbSubtitle = new JCheckBox();
        cbSubtitle.setEnabled(false);
        contentPane.add(cbSubtitle, "cell 1 8,wmax 250");

        label = new JLabel();
        label.setText("Geo:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, "cell 0 9");

        lblGeo = new JLabel();
        contentPane.add(lblGeo, "cell 1 9,wmax 250");

        label = new JLabel();
        label.setText("Abo:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, "cell 0 10");

        lblAbo = new JLabel();
        contentPane.add(lblAbo, "cell 1 10,wmax 250");

        label = new JLabel();
        label.setText("Website:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, "cell 0 11");

        btnLinkWebsite = new HyperlinkButton();
        btnLinkWebsite.setText("Hier klicken");
        contentPane.add(btnLinkWebsite, "cell 1 11,growx,wmax 250");

        label = new JLabel();
        label.setText("Bechreibung:");
        label.setHorizontalAlignment(SwingConstants.RIGHT);
        contentPane.add(label, "cell 0 12");

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
