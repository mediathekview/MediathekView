namespace java mediathek.tool.notification.thrift

enum MessageType {
	INFO,
	ERROR
}

struct ThriftNotificationMessage {
	1: required string title;
	2: required string message;
	3: required MessageType type;
}

service ThriftNotificationCenter {
	oneway void displayNotification(1: ThriftNotificationMessage message),
}