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
        // if so, transition to SelectPartState
        if( event instanceof NodePathEvent )
        {
            local node_path = event.GetNodePath();
            if( node_path != "" )
            {
                local node_path_array = split( node_path, "," );
                local index = node_path_array.len() - 1;
                return SelectPartState( node_path_array, index );
            }
        }

        return null;
    }

    m_logger = null;
}

class SelectPartState extends State
{
    constructor( node_path_array, index )
    {
        base.constructor();

        m_nodePathArray = node_path_array;
        m_nodePathArrayLength = m_nodePathArray.len();
        m_nodePathArrayIndex = index;

        m_logger = Logger();
    }

    function OnEnter( context )
    {
        m_logger.Info( "entered SelectPartState" );

        local path_slice = m_nodePathArray.slice( 0, m_nodePathArrayIndex + 1 );
        local path_string = path_slice.reduce( function( previous, current ) {
            return previous + "," + current;
        } );

        m_logger.Info( "Node path = " + path_string );
        HighlightNodeSignal.signal( path_string );
    }

    function OnEvent( context, event )
    {
        if( event instanceof NodePathEvent )
        {
            // check if an "empty" node path string was received
            // if so, transition back to IdleState
            // otherwise, remain in SelectPartState
            if( event.GetNodePath() == "" )
            {
                return IdleState();
            }
            else
            {
                local node_path_array = split( event.GetNodePath(), "," );
                local index = node_path_array.len() - 1;
                return SelectPartState( node_path_array, index );
            }
        }

        // check if gadget::HatState::UP was received
        // if so, transition to MovePartState
        if( event instanceof HatStateEvent )
        {
            local hat_state = event.GetHatState();

            switch( hat_state )
            {
                case HatState.UP:
                {
                    return MovePartState( GetNodePathForIndex( m_nodePathArrayIndex ) );
                    break;
                }
                case HatState.LEFT:
                {
                    // traverse up the node path
                    if( m_nodePathArrayIndex > 0 )
                    {
                        m_nodePathArrayIndex = m_nodePathArrayIndex - 1;

                        return SelectPartState( m_nodePathArray, m_nodePathArrayIndex );
                    }
                    break;
                }
                case HatState.RIGHT:
                {
                    // traverse down the node path
                    if( m_nodePathArrayIndex < m_nodePathArrayLength - 1 )
                    {
                        m_nodePathArrayIndex = m_nodePathArrayIndex + 1;

                        return SelectPartState( m_nodePathArray, m_nodePathArrayIndex );
                    }
                    break;
                }
                case HatState.DOWN:
                {
                    return IdleState();
                }
                default:
                    break;
            }
        }
      
        return null;
    }

    function GetNodePathForIndex( index )
    {
        local path_slice = m_nodePathArray.slice( 0, index + 1 );
        local path_string = path_slice.reduce( function( previous, current ) {
            return previous + "," + current;
        } );
        return path_string;
    }

    m_nodePathArray = null;
    m_nodePathArrayLength = null;
    m_nodePathArrayIndex = null;

    m_logger = null;
}

class MovePartState extends State
{
    constructor( node_path )
    {
        base.constructor();

        m_nodePath = node_path;

        m_logger = Logger();

        m_leftXAxisReceiver = FloatSynchronizedSignalReceiver();
        m_leftYAxisReceiver = FloatSynchronizedSignalReceiver();
        //m_rightXAxisReceiver = FloatSynchronizedSignalReceiver();
        m_rightYAxisReceiver = FloatSynchronizedSignalReceiver();

        m_L1ButtonReceiver = DigitalStateSynchronizedSignalReceiver();
        m_R1ButtonReceiver = DigitalStateSynchronizedSignalReceiver();

        m_set = PartManipulatorPropertySet();

        m_rotIncrement = 0;
    }

    function OnEnter( context )
    {
        m_logger.Info( "entered MovePartState" );
        m_leftXAxisReceiver.ConnectToSignal( "GameController.Axis0" );
        m_leftYAxisReceiver.ConnectToSignal( "GameController.Axis1" );
        //m_rightXAxisReceiver.ConnectToSignal( "GameController.Axis2" );
        m_rightYAxisReceiver.ConnectToSignal( "GameController.Axis3" );

        m_L1ButtonReceiver.ConnectToSignal( "GameController.Button4" );
        m_R1ButtonReceiver.ConnectToSignal( "GameController.Button5" );

        SetControlModeSignal.signal( ControlMode.USER_DEFINED );
        // TODO: record the original position of the selected object
        m_set.LoadByNodePath( m_nodePath );
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
        if( event instanceof HatStateEvent )
        {
            if( event.GetHatState() == HatState.DOWN )
            {
                return IdleState();
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
            m_set.SetTranslationX( m_set.GetTranslationX() + x_delta );
        }

        if( m_leftYAxisReceiver.Pending() )
        {
            // y axis gets inverted
            y_delta = -1.0 * (( 0.2 * m_leftYAxisReceiver.Pop() ) - 0.1);
            m_set.SetTranslationY( m_set.GetTranslationY() + y_delta );
        }

        if( m_rightYAxisReceiver.Pending() )
        {
            // y axis gets inverted
            z_delta = -1.0 * (( 0.2 * m_rightYAxisReceiver.Pop() ) - 0.1);
            m_set.SetTranslationZ( m_set.GetTranslationZ() + z_delta );
        }

        if( m_L1ButtonReceiver.Pending() )
        {
            local l1_button = m_L1ButtonReceiver.Pop();
            if( l1_button != m_L1PreviousState && l1_button == DigitalState.ON )
            {
                m_rotIncrement = ( m_rotIncrement + 1 ) % 8;
                //m_logger.Info( "Rotation Increment = " + m_rotIncrement );
                //m_logger.Info( "PI = " + PI );
                m_set.SetRotationZ( m_rotIncrement * ( PI / 4.0 ) );
            }
            m_L1PreviousState = l1_button;
        }

        if( m_R1ButtonReceiver.Pending() )
        {
            local r1_button = m_R1ButtonReceiver.Pop();
            if( r1_button != m_R1PreviousState && r1_button == DigitalState.ON )
            {
                m_rotIncrement = ( m_rotIncrement - 1 ) % 8;
                //m_logger.Info( "Rotation Increment = " + m_rotIncrement );
                m_set.SetRotationZ( m_rotIncrement * ( PI / 4.0 ) );
            }
            m_R1PreviousState = r1_button;
        }
    }

    m_set = null;

    m_nodePath = null;

    m_logger = null;

    m_leftXAxisReceiver = null;
    m_leftYAxisReceiver = null;
    //m_rightXAxisReceiver = null;
    m_rightYAxisReceiver = null;

    m_L1ButtonReceiver = null;
    m_R1ButtonReceiver = null;

    m_L1PreviousState = DigitalState.OFF;
    m_R1PreviousState = DigitalState.OFF;

    m_rotIncrement = null;
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
