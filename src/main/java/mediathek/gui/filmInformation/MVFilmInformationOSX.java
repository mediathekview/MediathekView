package mediathek.gui.filmInformation;

import mSearch.daten.DatenFilm;
import mediathek.config.Icons;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.tool.BeobMausUrl;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import java.awt.*;
import java.net.URISyntaxException;

/**
 * Display the current film information as a utility window.
 */
public class MVFilmInformationOSX implements IFilmInformation {

    private JDialog hudDialog = null;
    private JXHyperlink lblUrlThemaField;
    private JXHyperlink lblUrlSubtitle;
    private JTextArea textAreaBeschreibung;
    private JLabel jLabelFilmNeu;
    private JLabel jLabelFilmHD;
    private JLabel jLabelFilmUT;
    private JFrame parent = null;
    private final JLabel[] labelArrNames = new JLabel[DatenFilm.MAX_ELEM];
    private final JTextField[] txtArrCont = new JTextField[DatenFilm.MAX_ELEM];
    private DatenFilm aktFilm = new DatenFilm();
    private static final ImageIcon ja_sw_16 = Icons.ICON_DIALOG_EIN_SW;

    private void createDialog(JFrame parent) {
        hudDialog = new JDialog(parent);
        hudDialog.setTitle("Filminformation");
        hudDialog.setResizable(true);
        hudDialog.setType(Window.Type.UTILITY);
        hudDialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    }

    public MVFilmInformationOSX(JFrame owner) {
        parent = owner;

        createDialog(owner);
        final Border emptyBorder = BorderFactory.createEmptyBorder(0, 0, 0, 0);
        for (int i = 0; i < DatenFilm.MAX_ELEM; ++i) {
            final JLabel lbl = new JLabel(DatenFilm.COLUMN_NAMES[i] + ":");
            lbl.setHorizontalAlignment(SwingConstants.RIGHT);
            labelArrNames[i] = lbl;

            final JTextField tf = new JTextField("");
            tf.setEditable(false);
            tf.setBorder(emptyBorder);
            txtArrCont[i] = tf;
        }

        hudDialog.setContentPane(buildLayout());
        hudDialog.setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT);

        calculateHudPosition();
    }

    final private static int DEFAULT_WIDTH = 600;
    final private static int DEFAULT_HEIGHT = 450;

    private JComponent buildLayout() {
        JPanel panel = new JPanel();
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        lblUrlThemaField = new JXHyperlink();
        try {
            lblUrlThemaField.setAction(new UrlHyperlinkAction(parent, ""));
        } catch (URISyntaxException ignored) {
        }
        lblUrlThemaField.addMouseListener(new BeobMausUrl(lblUrlThemaField));

        lblUrlSubtitle = new JXHyperlink();
        try {
            lblUrlSubtitle.setAction(new UrlHyperlinkAction(parent, ""));
        } catch (URISyntaxException ignored) {
        }
        lblUrlSubtitle.addMouseListener(new BeobMausUrl(lblUrlSubtitle));
        textAreaBeschreibung = new JTextArea();
        textAreaBeschreibung.setLineWrap(true);
        textAreaBeschreibung.setWrapStyleWord(true);
        textAreaBeschreibung.setRows(4);

        jLabelFilmNeu = new JLabel();
        jLabelFilmNeu.setVisible(false);
        jLabelFilmNeu.setIcon(ja_sw_16);

        jLabelFilmHD = new JLabel();
        jLabelFilmHD.setVisible(false);
        jLabelFilmHD.setIcon(ja_sw_16);

        jLabelFilmUT = new JLabel();
        jLabelFilmUT.setVisible(false);
        jLabelFilmUT.setIcon(ja_sw_16);

        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(4, 10, 4, 10);
        c.weighty = 0;
        panel.setLayout(gridbag);
        int zeile = 0;
        for (int i = 0; i < labelArrNames.length; ++i) {
            if (i == DatenFilm.FILM_URL_RTMP
                    || i == DatenFilm.FILM_URL_AUTH
                    || i == DatenFilm.FILM_URL_HD
                    || i == DatenFilm.FILM_URL_RTMP_HD
                    || i == DatenFilm.FILM_URL_KLEIN
                    || i == DatenFilm.FILM_URL_RTMP_KLEIN
                    || i == DatenFilm.FILM_ABSPIELEN
                    || i == DatenFilm.FILM_AUFZEICHNEN
                    || i == DatenFilm.FILM_DATUM_LONG
                    || i == DatenFilm.FILM_URL_HISTORY
                    || i == DatenFilm.FILM_REF) {
                continue;
            }
            c.gridy = zeile;
            addComponentWithLayoutConstraints(i, gridbag, c, panel);
            ++zeile;
        }

        // zum zusammenschieben
        c.weightx = 0;
        c.gridx = 0;
        c.weighty = 2;
        c.gridy = zeile;
        JLabel label = new JLabel();
        gridbag.setConstraints(label, c);
        panel.add(label);
        return panel;
    }

    private void addComponentWithLayoutConstraints(int i, GridBagLayout gridbag, GridBagConstraints c, JPanel panel) {
        c.gridx = 0;
        c.weightx = 0;
        gridbag.setConstraints(labelArrNames[i], c);
        panel.add(labelArrNames[i]);
        c.gridx = 1;
        c.weightx = 10;
        switch (i) {
            case DatenFilm.FILM_WEBSEITE:
                gridbag.setConstraints(lblUrlThemaField, c);
                panel.add(lblUrlThemaField);
                break;
            case DatenFilm.FILM_URL_SUBTITLE:
                gridbag.setConstraints(lblUrlSubtitle, c);
                panel.add(lblUrlSubtitle);
                break;
            case DatenFilm.FILM_BESCHREIBUNG:
                gridbag.setConstraints(textAreaBeschreibung, c);
                panel.add(textAreaBeschreibung);
                break;
            case DatenFilm.FILM_NEU:
                gridbag.setConstraints(jLabelFilmNeu, c);
                panel.add(jLabelFilmNeu);
                break;
            case DatenFilm.FILM_HD:
                gridbag.setConstraints(jLabelFilmHD, c);
                panel.add(jLabelFilmHD);
                break;
            case DatenFilm.FILM_UT:
                gridbag.setConstraints(jLabelFilmUT, c);
                panel.add(jLabelFilmUT);
                break;
            default:
                gridbag.setConstraints(txtArrCont[i], c);
                panel.add(txtArrCont[i]);
                break;
        }
    }

    private void calculateHudPosition() {
        final GraphicsDevice gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
        final DisplayMode dm = gd.getDisplayMode();
        hudDialog.setLocation(dm.getWidth() - DEFAULT_WIDTH, 0);
    }

    @Override
    public void showInfo() {
        updateTextFields();
        hudDialog.setVisible(true);
    }

    @Override
    public boolean isVisible() {
        return hudDialog.isVisible();
    }

    @Override
    public void updateCurrentFilm(DatenFilm film) {
        aktFilm = film;
        if (isVisible()) {
            updateTextFields();
        }
    }

    private void clearAllFields() {
        for (JTextField aTxtArrCont : txtArrCont) {
            aTxtArrCont.setText("");
        }
        textAreaBeschreibung.setText(" ");
        lblUrlThemaField.setText("");
        lblUrlSubtitle.setText("");
        jLabelFilmNeu.setVisible(false);
        jLabelFilmHD.setVisible(false);
        jLabelFilmUT.setVisible(false);
    }

    private void updateTextFields() {
        if (aktFilm == null) {
            clearAllFields();
        } else {
            for (int i = 0; i < txtArrCont.length; ++i) {
                txtArrCont[i].setText(aktFilm.arr[i]);
            }
            if (aktFilm.arr[DatenFilm.FILM_BESCHREIBUNG].isEmpty()) {
                // sonst müsste die Größe gesetzt werden
                textAreaBeschreibung.setText(" ");
            } else {
                textAreaBeschreibung.setText(aktFilm.arr[DatenFilm.FILM_BESCHREIBUNG]);
            }
            lblUrlThemaField.setText(aktFilm.arr[DatenFilm.FILM_WEBSEITE]);
            lblUrlSubtitle.setText(aktFilm.getUrlSubtitle());
            jLabelFilmNeu.setVisible(aktFilm.isNew());
            jLabelFilmHD.setVisible(aktFilm.isHD());
            jLabelFilmUT.setVisible(aktFilm.hasUT());
        }
        hudDialog.repaint();
    }

    @Override
    public void stateChanged(ChangeEvent changeEvent) {
        //Whenever there is a change event, reset HUD info to nothing
        DatenFilm emptyFilm = new DatenFilm();
        updateCurrentFilm(emptyFilm);
    }
}
