#pragma once

#include <Persistence/DataAbstractionLayer.h>

#include <vector>

namespace Persistence
{

class BufferBase : public DataAbstractionLayer
{
public:
    BufferBase();
    virtual ~BufferBase();
    enum BufferPriority{ LOWEST_PRIORITY, LOW_PRIORITY, NORMAL_PRIORITY, HIGH_PRIORITY, HIGHEST_PRIORITY };

    virtual void Buffer( std::vector< std::string > ids, BufferPriority priority = NORMAL_PRIORITY ) = 0;
};

} // namespace Persistence
