#ifndef SELECT_EFFECT_H
#define SELECT_EFFECT_H

#include "VE_Installer/include/VEConfig.h"

#include <osgFX/Export>
#include <osgFX/Effect>

namespace VE_SceneGraph
{
namespace Utilities
{
class VE_SCENEGRAPH_UTILS_EXPORTS SelectEffect : public osgFX::Effect
{
public:
    SelectEffect();
    SelectEffect( const SelectEffect& copy, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    META_Effect( 
        VE_SceneGraph::Utilities,
        SelectEffect,
        "SelectEffect",
        "This is a two-pass effect;"
        "the first pass renders a velvet-like diffuse color;"
        "the second pass renders a shell of the original object to simulate a glow.",
        "Josh Koch" );

protected:
    virtual ~SelectEffect();
    SelectEffect& operator=( const SelectEffect& ) { return *this; }

    bool define_techniques();

private:


};
}
}

#endif //SELECT_EFFECT_H
