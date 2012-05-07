#pragma once

#include <Persistence/Cache.h>

namespace Persistence
{

class NullCache: public Cache
{
public:
    NullCache(){}
    // No virtual destructor and no virtual methods since there is no reason to
    // derive from this class.
    void Buffer( std::vector< std::string > ids, BufferPriority priority = NORMAL_PRIORITY )
    {
        static_cast<BufferBase*>(m_child.get())->Buffer( ids, priority );
    }
};

}
