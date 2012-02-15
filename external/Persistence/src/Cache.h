#pragma once

#include "BufferBase.h"

namespace Persistence
{

class Cache : public BufferBase
{
public:
    Cache();
    virtual ~Cache();
    virtual void Buffer( std::vector< std::string > ids, BufferPriority priority = NORMAL_PRIORITY ) = 0;
};

} // namespace Persistence
