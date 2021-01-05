package mediathek.tool;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class SysMsg {

    private static final ObservableList<String> textProgramm = FXCollections.observableArrayList();
    private static final int MAX_STELLEN = 5;
    private static final String FUELL_ZEICHEN = "0";
    private static int zeilenNrProgramm;

    public static synchronized void playerMsg(String text) {
        playermeldung(new String[]{text});
    }

    private static void playermeldung(String[] texte) {
        final String z = "  >>";
        System.out.println(z + " " + texte[0]);
        notify(texte[0]);
        for (int i = 1; i < texte.length; ++i) {
            System.out.println(z + " " + texte[i]);
            notify(texte[i]);
        }
    }

    private static void notify(String zeile) {
        addText("[" + getNr(zeilenNrProgramm++) + "]   " + zeile);
    }

    private static String getNr(int nr) {
        String str = String.valueOf(nr);
        while (str.length() < MAX_STELLEN) {
            str = FUELL_ZEICHEN + str;
        }
        return str;
    }

    private synchronized static void addText(String texte) {
        if (textProgramm.size() > 50000) {
            textProgramm.remove(0, 30000);
        }
        textProgramm.add(texte + System.lineSeparator());
    }
}
