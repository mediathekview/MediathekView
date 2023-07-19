package mediathek.gui.dialogEinstellungen;

import mediathek.tool.ApplicationConfiguration;

import javax.swing.*;

public class DaysSpinner extends JSpinner {
    private final static String ALLE = " Alle ";

    public DaysSpinner() {
        SpinnerListModel daySpinnerModel = new SpinnerListModel(new Object[]{ALLE, "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                "12", "14", "16", "18", "20", "25", "30", "60", "90", "180", "365"});
        setModel(daySpinnerModel);
        ((JSpinner.DefaultEditor) getEditor()).getTextField().setEditable(false);
        addChangeListener(l -> {
            String s = getModel().getValue().toString();
            if (s.equals(ALLE)) {
                s = "0";
            }

            int num_days;
            try {
                num_days = Integer.parseInt(s);
            } catch (NumberFormatException e) {
                num_days = 0;
            }
            ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, num_days);
        });
        loadConfigValues();
    }

    private void loadConfigValues() {
        String s;
        final int num_days = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, 0);
        if (num_days == 0)
            s = ALLE;
        else
            s = Integer.toString(num_days);

        setValue(s);
    }

}
