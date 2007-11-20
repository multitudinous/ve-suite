#ifndef FUNNEL_H
#define FUNNEL_H

// --- OSG Includes --- //
#include <osg/Geode>

namespace Demo
{
class Funnel : public osg::Geode
{
public:
    Funnel();

protected:
    virtual ~Funnel();

public:
    Funnel( const Funnel& funnel, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    META_Node( Demo, Funnel );

    void CreateFunnel();

private:

};
}

#endif //FUNNEL_H
