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
package mediathek.daten;

import java.util.Collections;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.regex.Pattern;
import javax.swing.JOptionPane;
import mediathek.Daten;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.tool.DatumZeit;
import mediathek.tool.TModelAbo;

public class ListeAbo extends LinkedList<DatenAbo> {

    DDaten daten;

    public ListeAbo(DDaten ddaten) {
        daten = ddaten;
    }
    private int nr = 0;

    public boolean addAbo(String filmSender, String filmThema, String filmTitel, String filmThemaTitel, String namePfad) {
        //abo anlegen, oder false wenns schon existiert
        boolean ret = false;
        DatenAbo datenAbo = new DatenAbo(namePfad /* name */, filmSender, filmThema, filmTitel, filmThemaTitel, namePfad, "");
        DialogEditAbo dialogEditAbo = new DialogEditAbo(null, true, daten, datenAbo);
        dialogEditAbo.setVisible(true);
        if (dialogEditAbo.ok) {
            if (getAbo(filmSender, filmThema, filmTitel) == null) {
                addAbo(datenAbo);
                sort();
                ret = true;
                Daten.notifyMediathekListener(MediathekListener.EREIGNIS_LISTE_ABOS, ListeAbo.class.getSimpleName());
            } else {
                JOptionPane.showMessageDialog(null, "Abo existiert bereits", "Abo anlegen", JOptionPane.INFORMATION_MESSAGE);
            }
        }
        return ret;
    }

    public boolean addAbo(String filmSender, String filmThema, String filmTitel) {
        return addAbo(filmSender, filmThema, filmTitel, "", filmThema);

    }

    public void aboLoeschen(DatenAbo abo) {
        if (abo != null) {
            this.remove(abo);
            Daten.notifyMediathekListener(MediathekListener.EREIGNIS_LISTE_ABOS, ListeAbo.class.getSimpleName());
        }
    }

    public void addAbo(DatenAbo datenAbo) {
        String str = String.valueOf(nr++);
        while (str.length() < 3) {
            str = "0" + str;
        }
        datenAbo.arr[DatenAbo.ABO_NR_NR] = str;
        //für die neue Funktion
//        if (datenAbo.arr[DatenAbo.ABO_THEMA_EXAKT_NR].equals("")) {
//            datenAbo.arr[DatenAbo.ABO_THEMA_EXAKT_NR] = Boolean.toString(true);
//        }
        super.add(datenAbo);
        Daten.notifyMediathekListener(MediathekListener.EREIGNIS_LISTE_ABOS, ListeAbo.class.getSimpleName());
    }

    public DatenAbo getAboNr(int i) {
        return this.get(i);
    }

    public void sort() {
        Collections.<DatenAbo>sort(this);
    }

    public void addObjectData(TModelAbo model) {
        Object[] object;
        DatenAbo datenAbo;
        model.setRowCount(0);
        ListIterator<DatenAbo> iterator = this.listIterator();
        object = new Object[DatenAbo.ABO_MAX_ELEM];
        while (iterator.hasNext()) {
            datenAbo = iterator.next();
            //object[i] = datenAbo.arr;
            for (int m = 0; m < DatenAbo.ABO_MAX_ELEM; ++m) {
                if (m == DatenAbo.ABO_DOWN_DATUM_NR) {
                    object[m] = DatumZeit.getDatumForObject(datenAbo.arr[DatenAbo.ABO_DOWN_DATUM_NR]);
                } else if (m == DatenAbo.ABO_EINGESCHALTET_NR) {
                    object[m] = ""; //Boolean.valueOf(datenAbo.aboIstEingeschaltet());
                } else {
                    object[m] = datenAbo.arr[m];
                }
            }
            model.addRow(object);
        }
    }

    public DatenAbo getAbo(String filmSender, String filmThema, String FilmTitel) {
        DatenAbo datenAbo;
        ListIterator<DatenAbo> it = this.listIterator();
        while (it.hasNext()) {
            datenAbo = it.next();
            if (filterAufAboPruefen(datenAbo.arr[DatenAbo.ABO_SENDER_NR], datenAbo.arr[DatenAbo.ABO_THEMA_NR], datenAbo.arr[DatenAbo.ABO_TITEL_NR],
                    datenAbo.arr[DatenAbo.ABO_THEMA_TITEL_NR], filmSender, filmThema, FilmTitel)) {
                return datenAbo;
            }
        }
        return null;
    }

    public static boolean filterAufAboPruefen(String aboFilter_SenderSuchen, String aboFilter_themaSuchen, String aboFilter_titelSuchen, String aboFilter_themaTitelSuchen,
            String imFilm_Sender, String imFilm_Thema, String imFilm_Titel) {
        // prüfen ob xxxSuchen im String imXxx enthalten ist, themaTitelSuchen wird mit Thema u. Titel verglichen
        // themaSuchen exakt mit thema
        // titelSuchen muss im Titel nur enthalten sein
        if (aboFilter_SenderSuchen.equals("") || imFilm_Sender.equalsIgnoreCase(aboFilter_SenderSuchen)) {
            if (aboFilter_themaSuchen.equals("") || imFilm_Thema.equalsIgnoreCase(aboFilter_themaSuchen)) {
                if (aboFilter_titelSuchen.equals("") || imFilm_Titel.toLowerCase().contains(aboFilter_titelSuchen.toLowerCase())) {
                    if (aboFilter_themaTitelSuchen.equals("")) {
                        return true;
                    } else if (pruefen(aboFilter_themaTitelSuchen, imFilm_Thema)) {
                        return true;
                    } else if (pruefen(aboFilter_themaTitelSuchen, imFilm_Titel)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private static boolean pruefen(String aboFilter, String im) {
        Pattern p = makePattern(aboFilter);
        if (p != null) {
            return (p.matcher(im).matches());
        } else if (!aboFilter.contains(" ")) {
            return (textPruefen(aboFilter, im));
        }
        String[] arr = getArr(aboFilter);
        for (int i = 0; i < arr.length; ++i) {
            if (textPruefen(arr[i], im)) {
                return true;
            }
        }
        return false;
    }

    private static String[] getArr(String str) {
        LinkedList<String> liste = new LinkedList<String>();
        String[] s;
        s = str.split(" ");
        for (int i = 0; i < s.length; ++i) {
            if (!s[i].equals("")) {
                liste.add(s[i]);
            }
        }
        return liste.toArray(new String[0]);
    }

    private static boolean textPruefen(String aboFilter_themaTitelSuchen, String imFilm) {
        if (aboFilter_themaTitelSuchen.equals("") || imFilm.toLowerCase().contains(aboFilter_themaTitelSuchen.toLowerCase())) {
            return true;
        } else {
            return false;
        }
    }

    public static boolean isPattern(String textSuchen) {
        return textSuchen.startsWith("#:");
    }

    public static Pattern makePattern(String textSuchen) {
        Pattern p = null;
        try {
            if (isPattern(textSuchen)) {
                p = Pattern.compile(textSuchen.substring(2));
            }
        } catch (Exception ex) {
            p = null;
        }
        return p;
    }
}
