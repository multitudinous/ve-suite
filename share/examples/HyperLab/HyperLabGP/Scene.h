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
    class Group;
    class Node;
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

private:
    void InitScene();

    void CreateNodes();
    void CreateLights();
    void CreateShadowTexture();
    void CreateJitterTexture();

    void WriteOutShadow();

    //Base Effects
    void Defaults();

    hyperlab::Shaders* shader;

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_pluginDCS;

    ves::xplorer::scenegraph::PhysicsSimulator* m_physicsSimulator;

    osg::ref_ptr< osg::Group > m_shadowedScene;

    osg::ref_ptr< osg::Texture2D > m_shadow;
    osg::ref_ptr< osg::Texture3D > m_jitter;
    osg::ref_ptr< osg::Camera > m_camera;
    osg::ref_ptr< osg::TexGenNode > m_texgenNode;

    //Variables to set up custom lighting for the scene
    osg::ref_ptr< osg::Light > m_light;
    osg::ref_ptr< osg::LightSource > m_lightSource;
    osg::ref_ptr< osg::MatrixTransform > m_lightTransform;

    //The room geometry nodes
    ves::xplorer::scenegraph::CADEntity* m_room;

    osg::ref_ptr< osg::Node > m_aluminumParts;
    osg::ref_ptr< osg::Node > m_aluminumPipes;
    osg::ref_ptr< osg::Node > m_black;
    osg::ref_ptr< osg::Node > m_brown;
    osg::ref_ptr< osg::Node > m_ceiling;
    osg::ref_ptr< osg::Node > m_details;
    osg::ref_ptr< osg::Node > m_floor;
    osg::ref_ptr< osg::Node > m_glass;
    osg::ref_ptr< osg::Node > m_ltGreen;
    osg::ref_ptr< osg::Node > m_ltGrey;
    osg::ref_ptr< osg::Node > m_orange;
    osg::ref_ptr< osg::Node > m_red;
    osg::ref_ptr< osg::Node > m_redBrown;
    osg::ref_ptr< osg::Node > m_walls;
    osg::ref_ptr< osg::Node > m_whitePipes;
    osg::ref_ptr< osg::Node > m_whiteDucts;
    osg::ref_ptr< osg::Node > m_yellow;

/*
    osg::ref_ptr< osg::Node > frame;
    osg::ref_ptr< osg::Node > railing;
    osg::ref_ptr< osg::Node > plenum_piping;
    osg::ref_ptr< osg::Node > blower_components;
    osg::ref_ptr< osg::Node > brackets;
    osg::ref_ptr< osg::Node > cement_base;
    osg::ref_ptr< osg::Node > combustor_piping;
    osg::ref_ptr< osg::Node > compressor_inlet;
    osg::ref_ptr< osg::Node > heat_exchanger;
    osg::ref_ptr< osg::Node > heat_exchanger_sweep;
    osg::ref_ptr< osg::Node > load;
    osg::ref_ptr< osg::Node > plenum_system;
    osg::ref_ptr< osg::Node > relief_piping;
    osg::ref_ptr< osg::Node > shell;
    osg::ref_ptr< osg::Node > stack;
    osg::ref_ptr< osg::Node > turbine_exhaust;
    osg::ref_ptr< osg::Node > turbine_postcombustor;
    osg::ref_ptr< osg::Node > miscellaneous;
    */
};
} //end hyperlab

#endif //SCENE_H
