#pragma once

#include <Persistence/BufferBase.h>

#include <vector>

namespace Persistence
{

class NullBuffer : public BufferBase
{
public:
    NullBuffer(){}

    void Buffer( std::vector< std::string > ids, BufferBase::BufferPriority priority = BufferBase::NORMAL_PRIORITY ){}
};

}
