#include <ves/xplorer/device/HatEventInterface.h>

namespace gadget
{

namespace event
{
HatSampleHandler::HatSampleHandler()
    : mCurState( HatState::CENTERED )
{
    /* Do nothing. */
}

const HatState::State& HatSampleHandler::getData( const sample_type& samples,
                                                  const unsigned int unit )
{
    mCurState = HatProxy::getNextState( mCurState,
                                        samples[unit].getValue() );
    return mCurState;
}
}
}
