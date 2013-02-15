#ifndef VES_XPLORER_DEVICE_GAME_CONTROLLER_PROXY_H
#define VES_XPLORER_DEVICE_GAME_CONTROLLER_PROXY_H
#include <ves/VEConfig.h>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

#include <switchwire/ScopedConnectionList.h>

#include <gadget/Type/PositionInterface.h>
#include <gadget/Event/AnalogEventInterface.h>
#include <gadget/Event/DigitalEventInterface.h>

namespace ves
{
namespace xplorer
{
namespace device
{

class GameControllerCallbacks;
    
/*!\file GameController.h
 * GameController API
 * \class ves::xplorer::device::GameController
 *
 */
class VE_XPLORER_EXPORTS GameController
{
public:
    ///Constructor
    GameController( unsigned int controllerID );
    
    ///Destructor
    ~GameController();

    ///Call init on all of the event interfaces listed above
    ///\param deviceName The VR Juggler name given for the PositionInterface
    void InitInterfaces( const std::string deviceName );
    ///Connect the event interfaces to the GameController class callbacks
    ///\param controller The GameController pointer
    void ConnectInterfaces( GameControllerCallbacks* const controller );
    ///Dicsonnect the event interfaces from the GameController class
    void DisconnectInterfaces( GameControllerCallbacks* const controller );

private:
    ///Check deadzone
    bool CheckDeadZone( const float& val ) const;
    
    ///Switchwire init list
    switchwire::ScopedConnectionList m_connections;

    ///The signature to tell others the game pad is active
    ves::util::UnsignedIntSignal_type m_grabControllSignal;
    
    ///Track the position of the game controller so that we can tell what the
    ///current orientation of the controller is for updating the
    ///up and forward vectors of the controller.
    gadget::PositionInterface m_gamecontroller;
    
    typedef gadget::AnalogEventInterface < gadget::event::all_events_tag,
        gadget::event::synchronized_tag > AnalogAxisInterface;
    AnalogAxisInterface m_analogAxis0EventInterface;
    AnalogAxisInterface m_analogAxis1EventInterface;
    AnalogAxisInterface m_analogAxis2EventInterface;
    AnalogAxisInterface m_analogAxis3EventInterface;
    AnalogAxisInterface m_analogAxis4EventInterface;
    AnalogAxisInterface m_analogAxis5EventInterface;
    
    typedef gadget::DigitalEventInterface < gadget::event::all_events_tag,
        gadget::event::synchronized_tag > GamePadClickInterface;
    GamePadClickInterface m_button0EventInterface;
    GamePadClickInterface m_button1EventInterface;
    GamePadClickInterface m_button2EventInterface;
    GamePadClickInterface m_button3EventInterface;
    GamePadClickInterface m_button4EventInterface;
    GamePadClickInterface m_button5EventInterface;
    GamePadClickInterface m_button6EventInterface;
    GamePadClickInterface m_button7EventInterface;
    GamePadClickInterface m_button8EventInterface;
    GamePadClickInterface m_button9EventInterface;
    GamePadClickInterface m_button10EventInterface;
    GamePadClickInterface m_button11EventInterface;
    GamePadClickInterface m_button12EventInterface;
    GamePadClickInterface m_button13EventInterface;
    GamePadClickInterface m_button14EventInterface;
    
    ///The id of this controller which maps to the m_gameControllerBaseNames
    //list of names.
    unsigned int m_controllerMask;
        
    /// All GameController events get delivered here
    void OnAxis0Event( const float event );
    /// All GameController events get delivered here
    void OnAxis1Event( const float event );
    /// All GameController events get delivered here
    void OnAxis2Event( const float event );
    /// All GameController events get delivered here
    void OnAxis3Event( const float event );
    /// All GameController events get delivered here
    void OnAxis4Event( const float event );
    /// All GameController events get delivered here
    void OnAxis5Event( const float event );

    /// All GameController events get delivered here
    void OnButton0Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton1Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton2Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton3Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton4Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton5Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton6Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton7Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton8Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton9Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton10Event( gadget::DigitalState::State event );
    /// All GameController events get delivered here
    void OnButton11Event( gadget::DigitalState::State event );
};
} //end xplorer
} //end ves
}
#endif
