package mediathek.tool;

import mediathek.config.Konstanten;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.io.PrintWriter;
import java.io.StringWriter;

public class SwingErrorDialog {
    public static void showExceptionMessage(@NotNull Component parentComponent,
                                            @NotNull String messageText,
                                            @NotNull Exception exception) throws HeadlessException {

        StringWriter stringWriter = new StringWriter();
        exception.printStackTrace(new PrintWriter(stringWriter));

        JLabel message = new JLabel(messageText);
        message.setBorder(BorderFactory.createEmptyBorder(3, 0, 10, 0));

        JTextArea text = new JTextArea();
        text.setEditable(false);
        text.setFont(UIManager.getFont("Label.font"));
        text.setText(stringWriter.toString());
        text.setCaretPosition(0);

        JScrollPane scroller = new JScrollPane(text);
        scroller.setPreferredSize(new Dimension(640, 350));

        JPanel panel = new JPanel();
        panel.setLayout(new BorderLayout());

        panel.add(message, BorderLayout.NORTH);
        panel.add(scroller, BorderLayout.SOUTH);

        JOptionPane.showMessageDialog(parentComponent, panel, Konstanten.PROGRAMMNAME,
                JOptionPane.ERROR_MESSAGE);

    }
}
