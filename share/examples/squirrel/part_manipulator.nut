class State
{
    constructor() {}

    function OnEnter( context ) {}

    function OnExit( context ) {}

    function OnEvent( context, event ) {}

    function Update() {}
}

class IdleState extends State
{
    constructor()
    {
        base.constructor();
        m_logger = Logger();
    }

    function OnEnter( context )
    {
        m_logger.Info( "entered IdleState" );
    }

    function OnEvent( context, event )
    {
        // check if a node path event was received
        // if so, transition to ReadyState
        if( event instanceof NodePathEvent )
        {
            if( event.GetNodePath() != "" )
            {
                return ReadyState( event.GetNodePath() );
            }
        }

        return null;
    }

    m_logger = null;
}

class ReadyState extends State
{
    constructor( node_path )
    {
        base.constructor();
        m_nodePath = node_path;
        m_logger = Logger();
    }

    function OnEnter( context )
    {
        m_logger.Info( "entered ReadyState" );
    }

    function OnEvent( context, event )
    {
        if( event instanceof NodePathEvent )
        {
            // check if an "empty" node path string was received
            // if so, transition back to IdleState
            // otherwise, remain in ReadyState
            if( event.GetNodePath() == "" )
            {
                return IdleState();
            }
            else
            {
                return ReadyState( event.GetNodePath() );
            }
        }

        // check if gadget::HatState::UP was received
        // if so, transition to ActiveState
        if( event instanceof HatStateEvent )
        {
            if( event.GetHatState() == HatState.UP )
            {
                return ActiveState( m_nodePath );
            }
        }
      
        return null;
    }

    m_nodePath = null;
    m_logger = null;
}

class ActiveState extends State
{
    constructor( node_path )
    {
        base.constructor();

        m_nodePath = node_path;
        m_nodePathArray = split( m_nodePath, "," );
        m_nodePathArrayLength = m_nodePathArray.len();
        m_nodePathArrayIndex = m_nodePathArrayLength - 1;

        m_logger = Logger();

        m_leftXAxisReceiver = FloatSynchronizedSignalReceiver();
        m_leftYAxisReceiver = FloatSynchronizedSignalReceiver();
        //m_rightXAxisReceiver = FloatSynchronizedSignalReceiver();
        m_rightYAxisReceiver = FloatSynchronizedSignalReceiver();

        m_L1ButtonReceiver = DigitalStateSynchronizedSignalReceiver();
        m_R1ButtonReceiver = DigitalStateSynchronizedSignalReceiver();
    }

    function OnEnter( context )
    {
        m_logger.Info( "entered ActiveState" );
        m_leftXAxisReceiver.ConnectToSignal( "GameController.Axis0" );
        m_leftYAxisReceiver.ConnectToSignal( "GameController.Axis1" );
        //m_rightXAxisReceiver.ConnectToSignal( "GameController.Axis2" );
        m_rightYAxisReceiver.ConnectToSignal( "GameController.Axis3" );

        m_L1ButtonReceiver.ConnectToSignal( "GameController.Button4" );
        m_R1ButtonReceiver.ConnectToSignal( "GameController.Button5" );

        SetControlModeSignal.signal( ControlMode.USER_DEFINED );
        // TODO: record the original position of the selected object
    }

    function OnExit( context )
    {
        m_leftXAxisReceiver.Disconnect();
        m_leftYAxisReceiver.Disconnect();
        //m_rightXAxisReceiver.Disconnect();
        m_rightYAxisReceiver.Disconnect();

        m_L1ButtonReceiver.Disconnect();
        m_R1ButtonReceiver.Disconnect();

        SetControlModeSignal.signal( ControlMode.NAV );
    }

    function OnEvent( context, event )
    {
        // check if an "empty" node path string was received
        // if so, transition back to IdleState
        // otherwise, remain in ActiveState
        if( event instanceof NodePathEvent )
        {
            if( event.GetNodePath() == "" )
            {
                return IdleState();
            }
            else
            {
                return ActiveState( event.GetNodePath() );
            }
        }

        // check if gadget::HatState::UP was received
        // if so, transition to ReadyState
        if( event instanceof HatStateEvent )
        {
            if( event.GetHatState() == HatState.UP )
            {
                return ReadyState( m_nodePath );
            }

            if( event.GetHatState() == HatState.DOWN )
            {
                // TODO: reset the object back to its original position
            }
        }

        return null;
    }

    function Update()
    {
        // analog stick values are in the range [0, 1], with 0.5 meaning centered
        // use 2x - 1 to convert to the range [-1, 1]

        local x_delta = 0.0;
        local y_delta = 0.0;
        local z_delta = 0.0;

        if( m_leftXAxisReceiver.Pending() )
        {
            x_delta = ( 0.2 * m_leftXAxisReceiver.Pop() ) - 0.1;
        }

        if( m_leftYAxisReceiver.Pending() )
        {
            y_delta = ( 0.2 * m_leftYAxisReceiver.Pop() ) - 0.1;
        }

        if( m_rightYAxisReceiver.Pending() )
        {
            //m_logger.Info( "z axis" );
            z_delta = ( 0.2 * m_rightYAxisReceiver.Pop() ) - 0.1;
        }

        if( m_L1ButtonReceiver.Pending() )
        {
            local l1_button = m_L1ButtonReceiver.Pop();
            if( l1_button != m_L1PreviousState && l1_button == DigitalState.ON )
            {
                // traverse up node path
                if( m_nodePathArrayIndex > 0 )
                {
                    m_nodePathArrayIndex = m_nodePathArrayIndex - 1;

                    local new_path = GetNodePathForIndex( m_nodePathArrayIndex )
                    //m_logger.Info( "NEW NODE PATH: " + new_path );
                    HighlightNodeSignal.signal( new_path );
                }
            }
            m_L1PreviousState = l1_button;
        }

        if( m_R1ButtonReceiver.Pending() )
        {
            local r1_button = m_R1ButtonReceiver.Pop();
            if( r1_button != m_R1PreviousState && r1_button == DigitalState.ON )
            {
                // traverse down node path
                if( m_nodePathArrayIndex < m_nodePathArrayLength - 1 )
                {
                    m_nodePathArrayIndex = m_nodePathArrayIndex + 1;

                    local new_path = GetNodePathForIndex( m_nodePathArrayIndex )
                    //m_logger.Info( "NEW NODE PATH: " + new_path );
                    HighlightNodeSignal.signal( new_path );
                }
            }
            m_R1PreviousState = r1_button;
        }
    }

    function GetNodePathForIndex( index )
    {
        local path_slice = m_nodePathArray.slice( 0, index + 1 );
        local path_string = path_slice.reduce( function( previous, current ) {
            return previous + "," + current;
        } );
        return path_string;
    }

    m_nodePath = null;
    m_nodePathArray = null;
    m_nodePathArrayLength = null;
    m_nodePathArrayIndex = null;

    m_logger = null;

    m_leftXAxisReceiver = null;
    m_leftYAxisReceiver = null;
    //m_rightXAxisReceiver = null;
    m_rightYAxisReceiver = null;

    m_L1ButtonReceiver = null;
    m_R1ButtonReceiver = null;

    m_L1PreviousState = DigitalState.OFF;
    m_R1PreviousState = DigitalState.OFF;
}

class Context
{
    constructor( initial_state )
    {
        m_state = initial_state;
        m_state.OnEnter( this );
    }

    function HandleEvent( event )
    {
        if( m_state != null )
        {
            local new_state = m_state.OnEvent( this, event );
            if( new_state != null )
            {
                m_state.OnExit( this );
                m_state = new_state;
                m_state.OnEnter( this );
            }
        }
    }

    m_state = null;
}

class PartManipulator extends Context
{
    constructor( initial_state )
    {
        base.constructor( initial_state );

        m_nodePathReceiver = StringSynchronizedSignalReceiver();
        m_hatStateReceiver = HatStateSynchronizedSignalReceiver();

        m_nodePathReceiver.ConnectToSignal( "Selection.ObjectPickedSignalAsString" );
        m_hatStateReceiver.ConnectToSignal( "GameController.Hat0" );

        m_logger = Logger();
    }

    function Update()
    {
        if( m_nodePathReceiver.Pending() )
        {
            local node_path_event = NodePathEvent( m_nodePathReceiver.Pop() );
            m_logger.Info( "Node Path event: " + node_path_event.GetNodePath() );
            HandleEvent( node_path_event );
        }

        if( m_hatStateReceiver.Pending() )
        {
            local new_hat_state = m_hatStateReceiver.Pop();
            
            if( new_hat_state != m_previousHatState )
            {
                local hat_state_event = HatStateEvent( new_hat_state );

                HandleEvent( hat_state_event );
            }

            m_previousHatState = new_hat_state;
        }

        m_state.Update();
    }

    m_nodePathReceiver = null;
    m_hatStateReceiver = null;

    m_previousHatState = HatState.CENTERED;

    m_logger = null;
}

class Event
{
    constructor() {}
}

class NodePathEvent extends Event
{
    constructor( node_path )
    {
        base.constructor();
        m_nodePath = node_path;
    }

    function GetNodePath()
    {
        return m_nodePath;
    }

    m_nodePath = null;
}

class HatStateEvent extends Event
{
    constructor( hat_state )
    {
        base.constructor();
        m_hatState = hat_state;
    }

    function GetHatState()
    {
        return m_hatState;
    }

    m_hatState = null;
}

function Execute()
{
    ControlModeSignalMaker <- ControlModeSignal;
    ControlModeSignalMaker.RegisterSignal( "GameController.ControlMode", "SetControlModeSignal" );

    StringSignalMaker <- StringSignal;
    StringSignalMaker.RegisterSignal( "PartManipulator.HighlightNodeWithStringPath", "HighlightNodeSignal" );

    local part_manipulator = PartManipulator( IdleState() );

    local app_exit_signal_receiver = BoolSynchronizedSignalReceiver();
    app_exit_signal_receiver.ConnectToSignal( "ScriptingTab.Destroy" );

    while( 1 )
    {
        if( app_exit_signal_receiver.Pending() )
        {
            break;
        }

        Sleeper.Sleep( 100 );
        part_manipulator.Update();
    }

    app_exit_signal_receiver.Disconnect();
}
