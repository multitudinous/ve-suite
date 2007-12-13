#ifndef HYPER_LAB_GP_H
#define HYPER_LAB_GP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/cfdVEBaseClass.h>

// --- OSG Includes --- //
#include <osg/Group>
#include <osg/MatrixTransform>
#include <osg/Texture2D>
#include <osg/CameraNode>
#include <osg/TexGenNode>

// --- My Includes --- //
class Scene;

// --- C/C++ Libraries --- //
#include <string>

class VE_USER_PLUGIN_EXPORTS HyperLabGP : public ves::xplorer::plugin::cfdVEBaseClass
{
public:
    HyperLabGP();
    virtual ~HyperLabGP();
    virtual void InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS );
    virtual void PreFrameUpdate();
    virtual void SetCurrentCommand( ves::open::xml::Command* command );

private:
    void UpdateParams();

    std::string _portNumber;
    std::string _excelData;

    unsigned int _phongID;
    unsigned int _textureID;
    unsigned int _shadowID;
    unsigned int _reflectionID;
    unsigned int _xrayID;

    double _ar_color;
    double _ag_color;
    double _ab_color;
    double _dr_color;
    double _dg_color;
    double _db_color;
    double _sr_color;
    double _sg_color;
    double _sb_color;

    Scene* root;

};

extern "C"
{
    VE_USER_PLUGIN_EXPORTS void* CreateVEPlugin()
    {
        return new HyperLabGP();
    }
}

#endif //HYPER_LAB_GP_H
   