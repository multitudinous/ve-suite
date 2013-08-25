#include <ves/xplorer/device/GameController.h>

#include <ves/xplorer/device/GameControllerCallbacks.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

namespace ves
{
namespace xplorer
{
namespace device
{
////////////////////////////////////////////////////////////////////////////////
GameController::GameController( unsigned int controllerID )
    :
    m_controllerMask( controllerID ),
    m_deadZone( 0.075 )
{
    switchwire::EventManager::instance()->RegisterSignal( ( &m_grabControllSignal ),
        "GameController" + boost::lexical_cast< std::string >( controllerID ) + ".GrabControllerState" );

    CONNECTSIGNALS_1( "%AnalogAxisDeadZoneChanged", void( const double& ),
                     &GameController::SetAnalogAxisDeadZone,
                     m_connections, any_SignalType, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
GameController::~GameController()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GameController::InitInterfaces( const std::string deviceName )
{
    //Left stick - X
    m_analogAxis0EventInterface.init( deviceName + "Axis0" );
    
    //Left stick - Y
    m_analogAxis1EventInterface.init( deviceName + "Axis1" );
    
    //Right stick - X
    m_analogAxis2EventInterface.init( deviceName + "Axis2" );
    
    //Right stick - Y
    m_analogAxis3EventInterface.init( deviceName + "Axis3" );
    
    m_analogAxis4EventInterface.init( deviceName + "Axis4" );
    
    m_analogAxis5EventInterface.init( deviceName + "Axis5" );
    
    //All the buttons
    m_button0EventInterface.init( deviceName + "Digital0" );
    
    m_button1EventInterface.init( deviceName + "Digital1" );
    
    m_button2EventInterface.init( deviceName + "Digital2" );
    
    m_button3EventInterface.init( deviceName + "Digital3" );
    
    m_button4EventInterface.init( deviceName + "Digital4" );
    
    m_button5EventInterface.init( deviceName + "Digital5" );
    
    m_button6EventInterface.init( deviceName + "Digital6" );
    
    m_button7EventInterface.init( deviceName + "Digital7" );
    
    m_button8EventInterface.init( deviceName + "Digital8" );
    
    m_button9EventInterface.init( deviceName + "Digital9" );
    
    m_button10EventInterface.init( deviceName + "Digital10" );
    
    m_button11EventInterface.init( deviceName + "Digital11" );
    
    //Setup the position events
    m_controllerPosition.init( deviceName + "Position" );
    
    m_controllerPosition.addCallback( boost::bind( &GameController::OnPositionEvent, this, _1 ) );

    _rumble.init( deviceName + "Rumble0");
    //std::cout << deviceName + "Rumble0 " << _rumble->isStupefied() << std::endl;

    //Left stick - X
    m_analogAxis0EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameController::OnAxis0Event, this, _1 ) );
    
    //Left stick - Y
    m_analogAxis1EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameController::OnAxis1Event, this, _1 ) );
    
    //Right stick - X
    m_analogAxis2EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameController::OnAxis2Event, this, _1 ) );
    
    //Right stick - Y
    m_analogAxis3EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameController::OnAxis3Event, this, _1 ) );

    if( m_analogAxis4EventInterface.isConnected() )
        m_analogAxis4EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameController::OnAxis4Event, this, _1 ) );
    if( m_analogAxis5EventInterface.isConnected() )    
        m_analogAxis5EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameController::OnAxis5Event, this, _1 ) );
    
    //All the buttons
    m_button0EventInterface.addCallback( boost::bind( &GameController::OnButton0Event, this, _1 ) );
    
    m_button1EventInterface.addCallback( boost::bind( &GameController::OnButton1Event, this, _1 ) );
    
    m_button2EventInterface.addCallback( boost::bind( &GameController::OnButton2Event, this, _1 ) );
    
    m_button3EventInterface.addCallback( boost::bind( &GameController::OnButton3Event, this, _1 ) );
    
    m_button4EventInterface.addCallback( boost::bind( &GameController::OnButton4Event, this, _1 ) );
    
    m_button5EventInterface.addCallback( boost::bind( &GameController::OnButton5Event, this, _1 ) );
    
    m_button6EventInterface.addCallback( boost::bind( &GameController::OnButton6Event, this, _1 ) );
    
    m_button7EventInterface.addCallback( boost::bind( &GameController::OnButton7Event, this, _1 ) );
    
    m_button8EventInterface.addCallback( boost::bind( &GameController::OnButton8Event, this, _1 ) );
    
    m_button9EventInterface.addCallback( boost::bind( &GameController::OnButton9Event, this, _1 ) );
    
    m_button10EventInterface.addCallback( boost::bind( &GameController::OnButton10Event, this, _1 ) );
    
    m_button11EventInterface.addCallback( boost::bind( &GameController::OnButton11Event, this, _1 ) );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::ConnectInterfaces( device::GameControllerCallbacks* const controller )
{
    //Left stick - X
    m_analogAxis0EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameControllerCallbacks::OnAxis0Event, controller, _1 ) );
    
    //Left stick - Y
    m_analogAxis1EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameControllerCallbacks::OnAxis1Event, controller, _1 ) );
    
    //Right stick - X
    m_analogAxis2EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameControllerCallbacks::OnAxis2Event, controller, _1 ) );
    
    //Right stick - Y
    m_analogAxis3EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameControllerCallbacks::OnAxis3Event, controller, _1 ) );
    
    if( m_analogAxis4EventInterface.isConnected() )  
        m_analogAxis4EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameControllerCallbacks::OnAxis4Event, controller, _1 ) );
    
    if( m_analogAxis5EventInterface.isConnected() )  
        m_analogAxis5EventInterface.addCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameControllerCallbacks::OnAxis5Event, controller, _1 ) );
    
      //Setup Rumble
     _speed = _rumble->createEffect(gadget::RumbleEffect::SINE);
     //std::cout << "*********************Try to create....****************" << std::endl;
     if( _speed )
     {
         std::cout << "Have a rumble effect " << _speed->load() << std::endl;
     }

    //All the buttons
    m_button0EventInterface.addCallback( boost::bind( &GameControllerCallbacks::OnButton0Event, controller, _1 ) );
    
    m_button1EventInterface.addCallback( boost::bind( &GameControllerCallbacks::OnButton1Event, controller, _1 ) );
    
    m_button2EventInterface.addCallback( boost::bind( &GameControllerCallbacks::OnButton2Event, controller, _1 ) );
    
    m_button3EventInterface.addCallback( boost::bind( &GameControllerCallbacks::OnButton3Event, controller, _1 ) );
    
    m_button4EventInterface.addCallback( boost::bind( &GameControllerCallbacks::OnButton4Event, controller, _1 ) );
    
    m_button5EventInterface.addCallback( boost::bind( &GameControllerCallbacks::OnButton5Event, controller, _1 ) );
    
    m_button6EventInterface.addCallback( boost::bind( &GameControllerCallbacks::OnButton6Event, controller, _1 ) );
    
    m_button7EventInterface.addCallback( boost::bind( &GameControllerCallbacks::OnButton7Event, controller, _1 ) );
    
    m_button8EventInterface.addCallback( boost::bind( &GameControllerCallbacks::OnButton8Event, controller, _1 ) );
    
    m_button9EventInterface.addCallback( boost::bind( &GameControllerCallbacks::OnButton9Event, controller, _1 ) );
    
    m_button10EventInterface.addCallback( boost::bind( &GameControllerCallbacks::OnButton10Event, controller, _1 ) );
    
    m_button11EventInterface.addCallback( boost::bind( &GameControllerCallbacks::OnButton11Event, controller, _1 ) );
    
    //Position events
    m_controllerPosition.addCallback( boost::bind( &GameControllerCallbacks::OnPositionEvent, controller, _1 ) );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::DisconnectInterfaces( device::GameControllerCallbacks* const controller )
{
    //Left stick - X
    m_analogAxis0EventInterface.removeCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameControllerCallbacks::OnAxis0Event, controller, _1 ) );
    
    //Left stick - Y
    m_analogAxis1EventInterface.removeCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameControllerCallbacks::OnAxis1Event, controller, _1 ) );
    
    //Right stick - X
    m_analogAxis2EventInterface.removeCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameControllerCallbacks::OnAxis2Event, controller, _1 ) );
    
    //Right stick - Y
    m_analogAxis3EventInterface.removeCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameControllerCallbacks::OnAxis3Event, controller, _1 ) );
    
    m_analogAxis4EventInterface.removeCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameControllerCallbacks::OnAxis4Event, controller, _1 ) );
    
    m_analogAxis5EventInterface.removeCallback<gadget::event::normalized_analog_event_tag>( boost::bind( &GameControllerCallbacks::OnAxis5Event, controller, _1 ) );
    
    //All the buttons
    m_button0EventInterface.removeCallback<gadget::event::digital_event_tag>( boost::bind( &GameControllerCallbacks::OnButton0Event, controller, _1 ) );
    
    m_button1EventInterface.removeCallback<gadget::event::digital_event_tag>( boost::bind( &GameControllerCallbacks::OnButton1Event, controller, _1 ) );
    
    m_button2EventInterface.removeCallback<gadget::event::digital_event_tag>( boost::bind( &GameControllerCallbacks::OnButton2Event, controller, _1 ) );
    
    m_button3EventInterface.removeCallback<gadget::event::digital_event_tag>( boost::bind( &GameControllerCallbacks::OnButton3Event, controller, _1 ) );
    
    m_button4EventInterface.removeCallback<gadget::event::digital_event_tag>( boost::bind( &GameControllerCallbacks::OnButton4Event, controller, _1 ) );
    
    m_button5EventInterface.removeCallback<gadget::event::digital_event_tag>( boost::bind( &GameControllerCallbacks::OnButton5Event, controller, _1 ) );
    
    m_button6EventInterface.removeCallback<gadget::event::digital_event_tag>( boost::bind( &GameControllerCallbacks::OnButton6Event, controller, _1 ) );
    
    m_button7EventInterface.removeCallback<gadget::event::digital_event_tag>( boost::bind( &GameControllerCallbacks::OnButton7Event, controller, _1 ) );
    
    m_button8EventInterface.removeCallback<gadget::event::digital_event_tag>( boost::bind( &GameControllerCallbacks::OnButton8Event, controller, _1 ) );
    
    m_button9EventInterface.removeCallback<gadget::event::digital_event_tag>( boost::bind( &GameControllerCallbacks::OnButton9Event, controller, _1 ) );
    
    m_button10EventInterface.removeCallback<gadget::event::digital_event_tag>( boost::bind( &GameControllerCallbacks::OnButton10Event, controller, _1 ) );
    
    m_button11EventInterface.removeCallback<gadget::event::digital_event_tag>( boost::bind( &GameControllerCallbacks::OnButton11Event, controller, _1 ) );

    //Position events
    m_controllerPosition.addCallback( boost::bind( &GameControllerCallbacks::OnPositionEvent, controller, _1 ) );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnPositionEvent( gmtl::Matrix44f )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis0Event( const float val )
{
    if( CheckDeadZone( val ) )
    {
        return;
    }

     if (_speed) {
     _speed->setMagnitude(1.0-val);
     _speed->update();
     _speed->play(-1);
     }
    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis1Event( const float val )
{
    if( CheckDeadZone( val ) )
    {
        return;
    }

    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis2Event( const float val )
{
    if( CheckDeadZone( val ) )
    {
        return;
    }

    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis3Event( const float val )
{
    if( CheckDeadZone( val ) )
    {
        return;
    }

    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis4Event( const float val )
{
    if( CheckTriggerDeadZone( val ) )
    {
        return;
    }

    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnAxis5Event( const float val )
{
    if( CheckTriggerDeadZone( val ) )
    {
        return;
    }

    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnButton0Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    
    if (_speed) {
     _speed->setMagnitude(1.0);
     _speed->update();
     _speed->play(-1);
     }
     
    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnButton1Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnButton2Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnButton3Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnButton4Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnButton5Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnButton6Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnButton7Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnButton8Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnButton9Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnButton10Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
void GameController::OnButton11Event( gadget::DigitalState::State event )
{
    if( event == gadget::DigitalState::OFF )
    {
        return;
    }
    m_grabControllSignal.signal( m_controllerMask );
}
////////////////////////////////////////////////////////////////////////////////
bool GameController::CheckDeadZone( const float& val ) const
{
    if( val < 0.5 + m_deadZone )
    {
        if( val > 0.5 - m_deadZone )
        {
            return true;
        }
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool GameController::CheckTriggerDeadZone( const float& val ) const
{
    if( val < 0.01f )
    {
        return true;
    }
    
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void GameController::SetAnalogAxisDeadZone( const double& val )
{
    m_deadZone = val;
}
////////////////////////////////////////////////////////////////////////////////
}
}
}