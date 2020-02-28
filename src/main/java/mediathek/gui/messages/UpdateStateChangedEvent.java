package mediathek.gui.messages;

/**
 *
 * @author Klaus
 */
public class UpdateStateChangedEvent extends BaseEvent {
  private final boolean new_state;
  
  public UpdateStateChangedEvent(boolean new_state) {
    this.new_state = new_state;
  }
  
  public boolean isActive() {
    return new_state;
  }
}
