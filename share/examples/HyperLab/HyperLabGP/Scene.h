#ifndef SCENE_H
#define SCENE_H

/// --- My Includes --- ///
namespace hyperlab
{
    class Shaders;
}

/// --- VE-Suite Includes --- //
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
    class DCS;
    class CADEntity;
    class PhysicsSimulator;
}
}
}

// --- OSG Includes --- //
namespace osg
{
    class Node;
    class Group;
    class Geode;
    class MatrixTransform;
    class Texture2D;
    class Texture3D;
    class Camera;
    class TexGenNode;
    class Light;
    class LightSource;
}

namespace hyperlab
{
class Scene
{
public:
    ///Constructor
    Scene( ves::xplorer::scenegraph::DCS* pluginDCS,
           ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    ///Destructor
    ~Scene();

    //Base Effects
    void DefaultVisuals();

    //Advanced Effects
    void AdvancedVisuals();

    //XRay Effect
    void XRay();

    osg::Light* GetLight();

private:
    void InitScene();

    void CreateNodes();
    void CreateLights();
    void CreateShadowTexture();
    void CreateJitterTexture();

    hyperlab::Shaders* shader;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_pluginDCS;

    ves::xplorer::scenegraph::PhysicsSimulator* m_physicsSimulator;

    //The room geometry nodes
    ves::xplorer::scenegraph::CADEntity* m_room;

    osg::ref_ptr< osg::Node > m_aluminumParts;
    osg::ref_ptr< osg::Node > m_aluminumPipes;
    osg::ref_ptr< osg::Node > m_black;
    osg::ref_ptr< osg::Node > m_brown;
    osg::ref_ptr< osg::Node > m_ceiling;
    osg::ref_ptr< osg::Geode > m_coronas;
    osg::ref_ptr< osg::Node > m_details;
    osg::ref_ptr< osg::Node > m_floor;
    osg::ref_ptr< osg::Node > m_glass;
    osg::ref_ptr< osg::Node > m_lights;
    osg::ref_ptr< osg::Node > m_ltGreen;
    osg::ref_ptr< osg::Node > m_ltGrey;
    osg::ref_ptr< osg::Node > m_orange;
    osg::ref_ptr< osg::Node > m_red;
    osg::ref_ptr< osg::Node > m_redBrown;
    osg::ref_ptr< osg::Node > m_walls;
    osg::ref_ptr< osg::Node > m_whiteDucts;
    osg::ref_ptr< osg::Node > m_whitePipes;
    osg::ref_ptr< osg::Node > m_yellow;
    
    osg::ref_ptr< osg::Node > m_blowerComponents;
    osg::ref_ptr< osg::Node > m_brackets;
    osg::ref_ptr< osg::Node > m_cableTray;
    osg::ref_ptr< osg::Node > m_cementBase;
    osg::ref_ptr< osg::Node > m_combustorInternals;
    osg::ref_ptr< osg::Node > m_combustorPiping;
    osg::ref_ptr< osg::Node > m_compressorInlet;
    osg::ref_ptr< osg::Node > m_frame;
    osg::ref_ptr< osg::Node > m_groundBolts;
    osg::ref_ptr< osg::Node > m_heatExchanger;
    osg::ref_ptr< osg::Node > m_heatExchangerSweep;
    osg::ref_ptr< osg::Node > m_instrumentation;
    osg::ref_ptr< osg::Node > m_load;
    osg::ref_ptr< osg::Node > m_plenumPiping;
    osg::ref_ptr< osg::Node > m_plenumSystem;
    osg::ref_ptr< osg::Node > m_railing;
    osg::ref_ptr< osg::Node > m_reliefPiping;
    osg::ref_ptr< osg::Node > m_reliefPipingAM;
    osg::ref_ptr< osg::Node > m_shell;
    osg::ref_ptr< osg::Node > m_stack;
    osg::ref_ptr< osg::Node > m_turbineExhaust;
    osg::ref_ptr< osg::Node > m_turbinePostCombustor;
    osg::ref_ptr< osg::Node > m_turbineSupport;

    //Variables to set up custom lighting for the scene
    osg::ref_ptr< osg::Light > m_light;
    osg::ref_ptr< osg::LightSource > m_lightSource;
    osg::ref_ptr< osg::MatrixTransform > m_lightTransform;

    //Variables to set up shadows for the scene
    osg::ref_ptr< osg::Group > m_shadowedScene;

    osg::ref_ptr< osg::Texture2D > m_shadow;
    osg::ref_ptr< osg::Texture3D > m_jitter;
    osg::ref_ptr< osg::Camera > m_camera;
    osg::ref_ptr< osg::TexGenNode > m_texgenNode;
};
} //end hyperlab

#endif //SCENE_H
