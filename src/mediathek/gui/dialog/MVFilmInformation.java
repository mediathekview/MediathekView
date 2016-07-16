package mediathek.gui.dialog;

import com.explodingpixels.macwidgets.HudWidgetFactory;
import com.explodingpixels.macwidgets.HudWindow;
import com.explodingpixels.macwidgets.plaf.HudPaintingUtils;
import java.awt.*;
import java.net.URISyntaxException;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import mSearch.daten.DatenFilm;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.tool.BeobMausUrl;
import mediathek.tool.EscBeenden;
import mediathek.tool.UrlHyperlinkAction;
import org.jdesktop.swingx.JXHyperlink;

/**
 * Display the current film information in a Apple-style HUD window.
 */
public class MVFilmInformation implements MVFilmInfo {

    private HudWindow hud = null;
    private JDialog dialog = null;
    private JXHyperlink lblUrlThemaField;
    private JXHyperlink lblUrlSubtitle;
    private JTextArea textAreaBeschreibung;
    private JLabel jLabelFilmNeu;
    private JLabel jLabelFilmHD;
    private JLabel jLabelFilmUT;
    private final JLabel[] labelArrNames = new JLabel[DatenFilm.MAX_ELEM];
    private final JTextField[] txtArrCont = new JTextField[DatenFilm.MAX_ELEM];
    private final Color foreground, background;
    private DatenFilm aktFilm = new DatenFilm();
    private final JFrame parent;
    private static ImageIcon ja_sw_16 = null;

    public MVFilmInformation(JFrame owner, JTabbedPane tabbedPane, Daten ddaten) {
        parent = owner;
        foreground = Color.WHITE;
        background = Color.BLACK;
        ja_sw_16 = Icons.ICON_DIALOG_EIN_SW;
        hud = new HudWindow("Filminformation", owner);
        hud.makeResizeable();
        for (int i = 0; i < DatenFilm.MAX_ELEM; ++i) {
            labelArrNames[i] = HudWidgetFactory.createHudLabel(DatenFilm.COLUMN_NAMES[i] + ":");
            labelArrNames[i].setHorizontalAlignment(SwingConstants.RIGHT);
            labelArrNames[i].setDoubleBuffered(true);
            txtArrCont[i] = HudWidgetFactory.createHudTextField("");
            txtArrCont[i].setEditable(false);
            txtArrCont[i].setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
            txtArrCont[i].setDoubleBuffered(true);
        }
        JComponent content = setLable();
        content.setDoubleBuffered(true);
        //prevents flickering in JDK7, JDK6 is still buggy :(
        content.setOpaque(false);
        hud.setContentPane(content);
        dialog = hud.getJDialog();
        // dialog.pack(); --> Exception in thread "AWT-EventQueue-0" sun.awt.X11.XException: Cannot write XdndAware property
        Dimension size = dialog.getSize();
        size.width = 600;
        size.height = 450;
        dialog.setSize(size);
        calculateHudPosition();

//        tabbedPane.addChangeListener(this);
        new EscBeenden(dialog) {
            @Override
            public void beenden_(JDialog d) {
                d.dispose();
            }
        };
    }

    private JComponent setLable() {
        JPanel panel = new JPanel();
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        lblUrlThemaField = new JXHyperlink();
        lblUrlThemaField.setDoubleBuffered(true);
        lblUrlThemaField.setForeground(foreground);
        try {
            lblUrlThemaField.setAction(new UrlHyperlinkAction(parent, ""));
        } catch (URISyntaxException ignored) {
        }
        lblUrlThemaField.addMouseListener(new BeobMausUrl(lblUrlThemaField));

        lblUrlThemaField.setFont(HudPaintingUtils.getHudFont());

        lblUrlSubtitle = new JXHyperlink();
        lblUrlSubtitle.setDoubleBuffered(true);
        lblUrlSubtitle.setForeground(foreground);
        try {
            lblUrlSubtitle.setAction(new UrlHyperlinkAction(parent, ""));
        } catch (URISyntaxException ignored) {
        }
        lblUrlSubtitle.addMouseListener(new BeobMausUrl(lblUrlSubtitle));
        lblUrlSubtitle.setFont(HudPaintingUtils.getHudFont());
        textAreaBeschreibung = new JTextArea();
        textAreaBeschreibung.setDoubleBuffered(true);
        textAreaBeschreibung.setLineWrap(true);
        textAreaBeschreibung.setWrapStyleWord(true);
        textAreaBeschreibung.setBackground(background);
        textAreaBeschreibung.setForeground(foreground);
        textAreaBeschreibung.setOpaque(false);
        textAreaBeschreibung.setRows(4);

        jLabelFilmNeu = new JLabel();
        jLabelFilmNeu.setOpaque(false);
        jLabelFilmNeu.setVisible(false);
        jLabelFilmNeu.setIcon(ja_sw_16);

        jLabelFilmHD = new JLabel();
        jLabelFilmHD.setOpaque(false);
        jLabelFilmHD.setVisible(false);
        jLabelFilmHD.setIcon(ja_sw_16);

        jLabelFilmUT = new JLabel();
        jLabelFilmUT.setOpaque(false);
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
            addLable(i, gridbag, c, panel);
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

    private void addLable(int i, GridBagLayout gridbag, GridBagConstraints c, JPanel panel) {
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
        //FIXME calculate the HUD position
    }

    @Override
    public void showInfo() {
        setAktFilm();
        dialog.setVisible(true);
    }

    @Override
    public boolean isVisible() {
        return dialog.isVisible();
    }

    @Override
    public void updateCurrentFilm(DatenFilm film) {
        aktFilm = film;
        if (this.isVisible()) {
            setAktFilm();
        }
    }

    private void setAktFilm() {
        lblUrlThemaField.setForeground(foreground);
        lblUrlSubtitle.setForeground(foreground);
        if (aktFilm == null) {
            for (JTextField aTxtArrCont : txtArrCont) {
                aTxtArrCont.setText("");
            }
            textAreaBeschreibung.setText(" ");
            lblUrlThemaField.setText("");
            lblUrlSubtitle.setText("");
            jLabelFilmNeu.setVisible(false);
            jLabelFilmHD.setVisible(false);
            jLabelFilmUT.setVisible(false);
        } else {
            for (int i = 0; i < txtArrCont.length; ++i) {
                txtArrCont[i].setText(aktFilm.arr[i]);
            }
            if (aktFilm.arr[DatenFilm.FILM_BESCHREIBUNG].equals("")) {
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
        dialog.repaint();
    }

    @Override
    public void stateChanged(ChangeEvent changeEvent) {
        //Whenever there is a change event, reset HUD info to nothing
        DatenFilm emptyFilm = new DatenFilm();
        updateCurrentFilm(emptyFilm);
    }

}
