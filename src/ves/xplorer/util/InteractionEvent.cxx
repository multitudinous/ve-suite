#include <ves/xplorer/util/InteractionEvent.h>

using namespace ves::xplorer::util;

InteractionEvent::InteractionEvent( eventType eType,
                                    char key,
                                    buttonType button,
                                    int buttons,
                                    float scrollDeltaX,
                                    float scrollDeltaZ,
                                    double x,
                                    double y,
                                    double z )
{
    EventType = eType;
    Key = key;
    Button = button;
    Buttons = buttons;
    ScrollDeltaX = scrollDeltaX;
    ScrollDeltaZ = scrollDeltaZ;
    X = x;
    Y = y;
    Z = z;
}

InteractionEvent::~InteractionEvent( )
{
}

