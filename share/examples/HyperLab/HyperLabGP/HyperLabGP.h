#ifndef HYPER_LAB_GP_H
#define HYPER_LAB_GP_H

// --- My Includes --- //
namespace hyperlab
{
    class Scene;
}

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/cfdVEBaseClass.h>

// --- OSG Includes --- //
#include <osg/Group>
#include <osg/MatrixTransform>
#include <osg/Texture2D>
#include <osg/CameraNode>
#include <osg/TexGenNode>

// --- C/C++ Libraries --- //
#include <string>

class VE_USER_PLUGIN_EXPORTS HyperLabGP : public ves::xplorer::plugin::cfdVEBaseClass
{
public:
    HyperLabGP();
    virtual ~HyperLabGP();
    virtual void InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS );
    virtual void PreFrameUpdate();
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );

private:
    void UpdateParams();

    std::string _portNumber;
    std::string _excelData;

    hyperlab::Scene* m_scene;
};

extern "C"
{
    VE_USER_PLUGIN_EXPORTS void* CreateVEPlugin()
    {
        return new HyperLabGP();
    }
}

#endif //HYPER_LAB_GP_H
   