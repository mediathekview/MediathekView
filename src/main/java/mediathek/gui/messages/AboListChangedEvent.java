package mediathek.gui.messages;

public class AboListChangedEvent extends BaseEvent {
  private final boolean StateEin;  // indicates if Abo(s) was activated
  
  public AboListChangedEvent(boolean newStateEin) {
    this.StateEin = newStateEin;
  }
  
  public boolean isStateEin() {
    return StateEin;
  }
}
