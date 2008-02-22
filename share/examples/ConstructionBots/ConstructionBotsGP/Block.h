#ifndef BLOCK_H
#define BLOCK_H

// --- OSG Includes --- //
#include <osg/Geode>

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
    bool m_isAttached;
};
}

#endif //BLOCK_H
