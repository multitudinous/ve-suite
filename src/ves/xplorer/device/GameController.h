#ifndef VES_XPLORER_DEVICE_GAME_CONTROLLER_PROXY_H
#define VES_XPLORER_DEVICE_GAME_CONTROLLER_PROXY_H
#include <ves/VEConfig.h>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

#include <switchwire/ScopedConnectionList.h>

#include <gadget/Event/AnalogEventInterface.h>
#include <gadget/Event/DigitalEventInterface.h>
#include <gadget/Event/PositionEventInterface.h>

#include <gadget/Type/RumbleInterface.h>

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
    ///Check deadzone for joysticks
    bool CheckDeadZone( const float& val ) const;
    ///Check deadzone for triggers
    bool CheckTriggerDeadZone( const float& val ) const;

    ///Set the deadzone value
    void SetAnalogAxisDeadZone( const double& val );

    ///Switchwire init list
    switchwire::ScopedConnectionList m_connections;

    ///The signature to tell others the game pad is active
    ves::util::UnsignedIntSignal_type m_grabControllSignal;
    
    ///Track the position of the game controller so that we can tell what the
    ///current orientation of the controller is for updating the
    ///up and forward vectors of the controller.
    typedef gadget::PositionEventInterface < gadget::event::all_events_tag,
        gadget::event::synchronized_tag > PositionInterface;
    PositionInterface m_controllerPosition;

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
    
    
    gadget::RumbleInterface _rumble;

	gadget::RumbleEffectPtr _speed;
	
    ///The id of this controller which maps to the m_gameControllerBaseNames
    //list of names.
    unsigned int m_controllerMask;
    
    ///The deadzone used to control the float value of the analog axis
    ///of game controllers 
    double m_deadZone;

    ///Callback for the position data from the controller
    void OnPositionEvent( gmtl::Matrix44f mat );
    
    /// All GameController events get delivered here
    ///\note This is the left stick
    void OnAxis0Event( const float event );
    /// All GameController events get delivered here
    ///\note This is the left stick
    void OnAxis1Event( const float event );
    /// All GameController events get delivered here
    ///\note This is the right stick
    void OnAxis2Event( const float event );
    /// All GameController events get delivered here
    ///\note This is the right stick
    void OnAxis3Event( const float event );
    /// All GameController events get delivered here
    ///\note This is the left trigger if it is analog
    void OnAxis4Event( const float event );
    /// All GameController events get delivered here
    ///\note This is the right trigger if it is analog
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
