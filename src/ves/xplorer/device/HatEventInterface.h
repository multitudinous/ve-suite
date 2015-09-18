#ifndef VES_XPLORER_DEVICE_HATEVENTINTERFACE_H
#define VES_XPLORER_DEVICE_HATEVENTINTERFACE_H

#include <gadget/gadgetConfig.h>

#include <gadget/Event/MultiEventInterface.h>
#include <gadget/Event/MultiEventGenerator.h>
#include <gadget/Type/HatProxy.h>

namespace gadget
{

namespace event
{

struct hat_event_tag : base_event_tag {};

class HatSampleHandler
{
public:
    typedef ProxyTraits<HatProxy>         device_type;
    typedef device_type::device_data_type device_data_type;
    typedef std::vector<device_data_type> sample_type;
    typedef device_type::raw_data_type    raw_data_type;

    HatSampleHandler();

    const HatState::State& getData( const sample_type& samples,
                                    const unsigned int unit );

private:
    HatState::State mCurState;
};
}

template< typename CollectionTag = event::last_event_tag,
          typename GenerationTag = event::synchronized_tag >
class HatEventInterface
    : public MultiEventInterface< HatProxy,
                                  MultiEventGenerator<
                                    HatProxy,
                                    boost::mpl::vector< event::hat_event_tag >,
                                    CollectionTag,
                                    GenerationTag,
                                    event::HatSampleHandler
                                  >
                                >
{
public:
    typedef typename HatEventInterface::event_interface_ base_type;

    using base_type::addCallback;

    void addCallback( const typename base_type::callback_type& callback )
    {
        base_type::template addCallback< event::hat_event_tag >( callback );
    }
};
}

#endif
