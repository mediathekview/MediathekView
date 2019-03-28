package mediathek.tool.notification;

import mSearch.tool.ApplicationConfiguration;
import mSearch.tool.javafx.FXErrorDialog;
import mediathek.config.Konstanten;
import mediathek.tool.notification.thrift.NotificationMessage;
import mediathek.tool.notification.thrift.ThriftNotificationCenter;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;

public class NativeNotificationCenter implements INotificationCenter {
    @Override
    public void displayNotification(NotificationMessage msg) {
        if (!ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NOTIFICATIONS,true))
            return;

        try (TTransport transport = new TSocket("localhost", 9090)){
            transport.open();

            TProtocol protocol = new TBinaryProtocol(transport);

            ThriftNotificationCenter.Client client = new ThriftNotificationCenter.Client(protocol);
            client.displayNotification(msg);
        } catch (TException e) {
            FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME,"Native Benachrichtigungen können nicht angezeigt werden",
                    "Bitte stellen Sie sicher das das Hilfsprogramm gestartet ist.", e);
        }
    }
}
