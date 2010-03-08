#ifndef _INTERACTIONEVENT_H
#define	_INTERACTIONEVENT_H

namespace ves
{
namespace xplorer
{
namespace util
{

class InteractionEvent
{
public:

    enum eventType
    {
        keyPress, keyRelease, buttonPress, buttonRelease, pointerMotion, scroll
    };

    enum buttonType
    {
        none = 0x0000, button_1 = 0x0001, button_2 = 0x0002, button_3 = 0x0004,
        button_4 = 0x0008, button_5 = 0x0010, button_6 = 0x0020,
        button_7 = 0x0040, button_8 = 0x0080, button_9 = 0x0100,
        button_10 = 0x0200, button_11 = 0x0400, button_12 = 0x0800,
        button_13 = 0x1000, button_14 = 0x2000, button_15 = 0x4000,
        button_16 = 0x8000
    };

    InteractionEvent( eventType eType,
                      char key,
                      buttonType button = none,
                      int buttons = none,
                      float scrollDeltaX = 0.0,
                      float scrollDeltaZ = 0.0,
                      double x = 0.0,
                      double y = 0.0,
                      double z = 0.0 );

    virtual ~InteractionEvent( );

    eventType EventType;
    char Key;
    buttonType Button;
    int Buttons;
    float ScrollDeltaX;
    float ScrollDeltaZ;
    double X;
    double Y;
    double Z;


private:

};
} //end util
} //end xplorer
} //end ves

#endif	/* _INTERACTIONEVENT_H */

