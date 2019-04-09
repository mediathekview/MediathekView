package mediathek.tool.notification;

import mSearch.tool.ApplicationConfiguration;
import mSearch.tool.javafx.FXErrorDialog;
import mediathek.config.Konstanten;
import mediathek.tool.notification.thrift.NotificationMessage;
import mediathek.tool.notification.thrift.ThriftNotificationCenter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;

import javax.jmdns.JmDNS;
import javax.jmdns.ServiceEvent;
import javax.jmdns.ServiceListener;
import java.net.InetAddress;

public class NativeNotificationCenter implements INotificationCenter, ServiceListener {
    private int serverPort;
    private InetAddress serverAddress;
    private static final Logger logger = LogManager.getLogger(NativeNotificationCenter.class);

    public NativeNotificationCenter() {
        setupJmdnsListener();
    }

    private void setupJmdnsListener() {
        try {
            JmDNS jmdns = JmDNS.create(InetAddress.getLocalHost());
            jmdns.addServiceListener("_mv-notification._tcp.local.", this);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    @Override
    public void displayNotification(NotificationMessage msg) {
        if (!ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NOTIFICATIONS, true))
            return;

        if (serverPort == -1 || serverAddress == null) {
            showErrorDialog(new IllegalArgumentException("server port or server address not set"));
            return;
        }

        logger.trace("Sending native notification to {} on serverPort {}", serverAddress, serverPort);
        try (TTransport transport = new TSocket(serverAddress.getHostAddress(), serverPort)) {
            transport.open();

            TProtocol protocol = new TBinaryProtocol(transport);

            ThriftNotificationCenter.Client client = new ThriftNotificationCenter.Client(protocol);
            client.displayNotification(msg);
        } catch (TException e) {
            showErrorDialog(e);
        }
    }

    private void showErrorDialog(Exception ex) {
        FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME, "Native Benachrichtigungen können nicht angezeigt werden",
                "Bitte stellen Sie sicher das das Hilfsprogramm gestartet ist.", ex);
    }

    @Override
    public void serviceAdded(ServiceEvent serviceEvent) {
        var info = serviceEvent.getInfo();
        serverAddress = info.getInetAddress();
        serverPort = info.getPort();
    }

    @Override
    public void serviceRemoved(ServiceEvent serviceEvent) {
        serverPort = -1;
        serverAddress = null;
    }

    @Override
    public void serviceResolved(ServiceEvent serviceEvent) {
        var info = serviceEvent.getInfo();
        serverAddress = info.getInetAddress();
        serverPort = info.getPort();
    }
}
