// a simple state machine representing a toggle button with two states - off and on
//
//   The state machine receives input in the form of a virtual button press.
//   Each button press switches the ToggleButton between the two states.
//
//   The actual state transition logic is implemented in the OnState and OffState
//   classes. State classes receive input from their parent context as an event,
//   and based on the event's content, return an instance of the next state
//   to transition to that state (or 'null' to remain in the same state).
//
//   See also: http://en.wikipedia.org/wiki/State_pattern
//
class ToggleButton extends StateMachine.Context
{
    constructor()
    {
        base.constructor();
    }
}

// a state class to represent the "on" state
class OnState extends StateMachine.State
{
    logger = null;

    constructor()
    {
        base.constructor();
        logger = Logger();
    }

    function onEnter(context)
    {
        logger.info("entering the 'on' state...");
    }

    function onExit(context)
    {
        logger.info("leaving the 'on' state...");
    }

    function onEvent(context, event)
    {
        // transition to the 'off' state
        return OffState();
    }
}

// a state class to represent the "off" state
class OffState extends StateMachine.State
{
    logger = null;

    constructor()
    {
        base.constructor();
        logger = Logger();
    }

    function onEnter(context)
    {
        logger.info("entering the 'off' state...");
    }

    function onExit(context)
    {
        logger.info("leaving the 'off' state...");
    }

    function onEvent(context, event)
    {
        // transition to the 'on' state
        return OnState();
    }
}

// a "dummy" event to represent pressing the toggle button
class ButtonPressEvent extends StateMachine.Event
{
    constructor()
    {
        base.constructor();
    }
}

function Execute()
{
    local toggle_button = ToggleButton();
    toggle_button.setInitialState( OffState() );

    local dummy_event = ButtonPressEvent();

    sleeper <- Sleeper;

    local log = Logger();

    while(1)
    {
       log.info("pressing the button...");
       toggle_button.handleEvent( dummy_event );
       sleeper.Sleep( 2000 );
    }
} 
