package mediathek.gui.filterpanel.filterpanel;

import mediathek.tool.FilterDTO;
import javax.swing.*;
import java.awt.Component;
import java.util.UUID;

public class FilterComboBoxSwing extends JComboBox<FilterDTO> {

    public FilterComboBoxSwing() {
        super();
        addItem(new FilterDTO(UUID.randomUUID(),"Hallo"));
        setSelectedIndex(0);

    }
}
