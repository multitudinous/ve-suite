class State
{
    constructor() {}

    function OnEnter( context ) {}

    function OnExit( context ) {}

    function OnEvent( context, event ) {}

    function Update() {}
}

class IdleState extends State//StateMachine.State
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
        // check if a node UUID event was received
        // if so, transition to ReadyState
        if( event instanceof NodeUUIDEvent )
        {
            if( event.GetNodeUUID() != "" )
            {
                return ReadyState( event.GetNodeUUID() );
            }
        }

        return null;
    }

    m_logger = null;
}

class ReadyState extends State//StateMachine.State
{
    constructor( node_uuid )
    {
        base.constructor();
        m_nodeUUID = node_uuid;
        m_logger = Logger();
    }

    function OnEnter( context )
    {
        m_logger.Info( "entered ReadyState" );
    }

    function OnEvent( context, event )
    {
        if( event instanceof NodeUUIDEvent )
        {
            // check if an "empty" UUID string was received
            // if so, transition back to IdleState
            // otherwise, remain in ReadyState
            if( event.GetNodeUUID() == "" )
            {
                return IdleState();
            }
            else
            {
                return ReadyState( event.GetNodeUUID() );
            }
        }

        // check if gadget::HatState::UP was received
        // if so, transition to ActiveState
        if( event instanceof HatStateEvent )
        {
            if( event.GetHatState() == HatState.UP )
            {
                return ActiveState( m_nodeUUID );
            }
        }
      
        return null;
    }

    m_nodeUUID = null;
    m_logger = null;
}

class ActiveState extends State//StateMachine.State
{
    constructor( node_uuid )
    {
        base.constructor();
        m_nodeUUID = node_uuid;
        m_logger = Logger();

        m_cadPropertySet = CADPropertySet();
        m_cadPropertySet.SetUUID( m_nodeUUID );
        m_cadPropertySet.Load();

        m_leftXAxisReceiver = FloatQueuedSignalReceiver();
        m_leftYAxisReceiver = FloatQueuedSignalReceiver();
        //m_rightXAxisReceiver = FloatQueuedSignalReceiver();
        m_rightYAxisReceiver = FloatQueuedSignalReceiver();
    }

    function OnEnter( context )
    {
        m_logger.Info( "entered ActiveState" );
        m_leftXAxisReceiver.ConnectToSignal( "GameController.Axis0" );
        m_leftYAxisReceiver.ConnectToSignal( "GameController.Axis1" );
        //m_rightXAxisReceiver.ConnectToSignal( "GameController.Axis2" );
        m_rightYAxisReceiver.ConnectToSignal( "GameController.Axis3" );

        // TODO: record the original position of the selected object
    }

    function OnExit( context )
    {
        m_leftXAxisReceiver.Disconnect();
        m_leftYAxisReceiver.Disconnect();
        //m_rightXAxisReceiver.Disconnect();
        m_rightYAxisReceiver.Disconnect();
    }

    function OnEvent( context, event )
    {
        // check if an "empty" UUID string was received
        // if so, transition back to IdleState
        // otherwise, remain in ActiveState
        if( event instanceof NodeUUIDEvent )
        {
            if( event.GetNodeUUID() == "" )
            {
                return IdleState();
            }
            else
            {
                return ActiveState( event.GetNodeUUID() );
            }
        }

        // check if gadget::HatState::UP was received
        // if so, transition to ReadyState
        if( event instanceof HatStateEvent )
        {
            if( event.GetHatState() == HatState.UP )
            {
                return ReadyState( m_nodeUUID );
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

        local leftX_delta = 0.0;
        local leftY_delta = 0.0;
        local rightY_delta = 0.0;

        if( m_leftXAxisReceiver.Pending() )
        {
            leftX_delta = ( 0.2 * m_leftXAxisReceiver.Pop() ) - 0.1;
        }

        if( m_leftYAxisReceiver.Pending() )
        {
            leftY_delta = ( 0.2 * m_leftYAxisReceiver.Pop() ) - 0.1;
        }

        if( m_rightYAxisReceiver.Pending() )
        {
            rightY_delta = ( 0.2 * m_rightYAxisReceiver.Pop() ) - 0.1;
        }

        // TODO: update the CADPropertySet with the position deltas
    }

    m_nodeUUID = null;
    m_cadPropertySet = null;
    m_logger = null;

    m_leftXAxisReceiver = null;
    m_leftYAxisReceiver = null;
    //m_rightXAxisReceiver = null;
    m_rightYAxisReceiver = null;
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

class PartManipulator extends Context//StateMachine.Context
{
    constructor( initial_state )
    {
        base.constructor( initial_state );

        m_nodeUUIDReceiver = StringQueuedSignalReceiver();
        m_hatStateReceiver = HatStateQueuedSignalReceiver();

        m_nodeUUIDReceiver.ConnectToSignal( "Selection.ObjectPickedNodeUUIDSignal" );
        m_hatStateReceiver.ConnectToSignal( "GameController.Hat0" );

        m_logger = Logger();
    }

    function Update()
    {
        if( m_nodeUUIDReceiver.Pending() )
        {
            local node_uuid_event = NodeUUIDEvent( m_nodeUUIDReceiver.Pop() );
            m_logger.Info( "UUID event: " + node_uuid_event.GetNodeUUID() );
            HandleEvent( node_uuid_event );   
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

    m_nodeUUIDReceiver = null;
    m_hatStateReceiver = null;

    m_previousHatState = HatState.CENTERED;

    m_logger = null;
}

class Event
{
    constructor() {}
}

class NodeUUIDEvent extends Event//StateMachine.Event
{
    constructor( node_uuid )
    {
        base.constructor();
        m_nodeUUID = node_uuid;
    }

    function GetNodeUUID()
    {
        return m_nodeUUID;
    }

    m_nodeUUID = null;
}

class HatStateEvent extends Event//StateMachine.Event
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
    //local part_manipulator = PartManipulator();
    //part_manipulator.SetInitialState( IdleState() );
    local part_manipulator = PartManipulator( IdleState() );

    while( 1 )
    {
        Sleeper.Sleep( 10 );
        part_manipulator.Update();
    }       
}
