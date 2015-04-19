package mediathek.gui.dialog;

import com.explodingpixels.macwidgets.HudWidgetFactory;
import com.explodingpixels.macwidgets.HudWindow;
import com.explodingpixels.macwidgets.plaf.HudPaintingUtils;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.net.URISyntaxException;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.daten.Daten;
import mediathek.res.GetIcon;
import mediathek.tool.BeobMausUrl;
import mediathek.tool.EscBeenden;
import mediathek.tool.UrlHyperlinkAction;
import msearch.daten.DatenFilm;
import org.jdesktop.swingx.JXHyperlink;

/**
 * Display the current film information in a Apple-style HUD window.
 */
public class MVFilmInformation implements ChangeListener {

    private HudWindow hud = null;
    private JDialog dialog = null;
    private JXHyperlink lblUrlThemaField;
    private JXHyperlink lblUrlSubtitle;
    private JTextArea textAreaBeschreibung;
    private JLabel jLabelFilmNeu;
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
        ja_sw_16 = GetIcon.getProgramIcon("ja_sw_16.png");
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

        tabbedPane.addChangeListener(this);
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
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(4, 10, 4, 10);
        c.weighty = 0;
        panel.setLayout(gridbag);
        int zeile = 0;
        for (int i = 0; i < labelArrNames.length; ++i) {
            if (i == DatenFilm.FILM_URL_RTMP_NR
                    || i == DatenFilm.FILM_URL_AUTH_NR
                    || i == DatenFilm.FILM_URL_HD_NR
                    || i == DatenFilm.FILM_URL_RTMP_HD_NR
                    || i == DatenFilm.FILM_URL_KLEIN_NR
                    || i == DatenFilm.FILM_URL_RTMP_KLEIN_NR
                    || i == DatenFilm.FILM_ABSPIELEN_NR
                    || i == DatenFilm.FILM_AUFZEICHNEN_NR
                    || i == DatenFilm.FILM_DATUM_LONG_NR
                    || i == DatenFilm.FILM_URL_HISTORY_NR
                    || i == DatenFilm.FILM_REF_NR) {
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
        if (i == DatenFilm.FILM_WEBSEITE_NR) {
            gridbag.setConstraints(lblUrlThemaField, c);
            panel.add(lblUrlThemaField);
        } else if (i == DatenFilm.FILM_URL_SUBTITLE_NR) {
            gridbag.setConstraints(lblUrlSubtitle, c);
            panel.add(lblUrlSubtitle);
        } else if (i == DatenFilm.FILM_BESCHREIBUNG_NR) {
            gridbag.setConstraints(textAreaBeschreibung, c);
            panel.add(textAreaBeschreibung);
        } else if (i == DatenFilm.FILM_NEU_NR) {
            gridbag.setConstraints(jLabelFilmNeu, c);
            panel.add(jLabelFilmNeu);
        } else {
            gridbag.setConstraints(txtArrCont[i], c);
            panel.add(txtArrCont[i]);
        }
    }

    private void calculateHudPosition() {
        //FIXME calculate the HUD position
    }

    public void show() {
        setAktFilm();
        dialog.setVisible(true);
    }

    public boolean isVisible() {
        return dialog.isVisible();
    }

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
        } else {
            for (int i = 0; i < txtArrCont.length; ++i) {
                txtArrCont[i].setText(aktFilm.arr[i]);
            }
            if (aktFilm.arr[DatenFilm.FILM_BESCHREIBUNG_NR].equals("")) {
                // sonst müsste die Größe gesetzt werden
                textAreaBeschreibung.setText(" ");
            } else {
                textAreaBeschreibung.setText(aktFilm.arr[DatenFilm.FILM_BESCHREIBUNG_NR]);
            }
            lblUrlThemaField.setText(aktFilm.arr[DatenFilm.FILM_WEBSEITE_NR]);
            lblUrlSubtitle.setText(aktFilm.arr[DatenFilm.FILM_URL_SUBTITLE_NR]);
            jLabelFilmNeu.setVisible(aktFilm.neuerFilm);
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
