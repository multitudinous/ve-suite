#ifndef BLOCK_H
#define BLOCK_H

// --- OSG Includes --- //
#include <osg/Geode>

namespace osg
{
    class Geometry;
}

// --- C/C++ Libraries --- //
#include <map>
#include <string>

namespace Construction
{
class Block : public osg::Geode
{
public:
    Block();

protected:
    virtual ~Block();

public:
    Block( const Block& block, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    META_Node( Construction, Block );

    void CreateBlock();

    void SetColor( float r, float g, float b, float a );

private:
    std::map< std::string, osg::ref_ptr< osg::Geometry > > m_sideStates;
};
}

#endif //BLOCK_H
