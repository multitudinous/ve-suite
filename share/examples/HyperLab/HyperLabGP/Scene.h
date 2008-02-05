#ifndef SCENE_H
#define SCENE_H

/// --- VE-Suite Includes --- //
class Shaders;

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
    class Material;
}

class Scene
{
public:
    Scene();
    ~Scene();

    osg::ref_ptr< osg::Group > InitScene();

    //Initial Effect
    void Base();

    //One effect
    void Phong();
    void Texture();
    void PCF();
    void Reflection();
    void XRay();

    //Two effect combos
    void PhongTexture();
    void PhongPCF();
    void PhongReflection();
    void TexturePCF();
    void TextureReflection();
    void PCFReflection();

    //Three effect combos
    void PhongTexturePCF();
    void PhongTextureReflection();
    void PhongPCFReflection();
    void TexturePCFReflection();

    //All effects
    void PhongTexturePCFReflection();

    void CreateLights();
    void CreateNodes();
    void Defaults();
    void WriteOutShadow();

    void CreateShadowTexture();
    void CreateJitterTexture();

    osg::ref_ptr< osg::Group > root;
    osg::ref_ptr< osg::Group > shadowed_scene;
    osg::ref_ptr< osg::Group > non_shadowed_scene;

    osg::ref_ptr< osg::Light > light_1;
    osg::ref_ptr< osg::LightSource > light_source_1;
    osg::ref_ptr< osg::MatrixTransform > light_transform_1;

    osg::ref_ptr< osg::Texture2D > shadow;
    osg::ref_ptr< osg::Texture3D > jitter;
    osg::ref_ptr< osg::Camera > camera;
    osg::ref_ptr< osg::TexGenNode > texgenNode;

    Shaders* shader;

    osg::ref_ptr< osg::Group > coronas;

    osg::ref_ptr< osg::Node > aluminum_pipes;
    osg::ref_ptr< osg::Node > aluminum_parts;
    osg::ref_ptr< osg::Node > black;
    osg::ref_ptr< osg::Node > brown;
    osg::ref_ptr< osg::Node > details;
    osg::ref_ptr< osg::Node > glass;
    osg::ref_ptr< osg::Node > lights;
    osg::ref_ptr< osg::Node > lt_green;
    osg::ref_ptr< osg::Node > lt_grey;
    osg::ref_ptr< osg::Node > orange;
    osg::ref_ptr< osg::Node > red;
    osg::ref_ptr< osg::Node > red_brown;
    osg::ref_ptr< osg::Node > ceiling;
    osg::ref_ptr< osg::Node > floor;
    osg::ref_ptr< osg::Node > walls;
    osg::ref_ptr< osg::Node > white_pipes;
    osg::ref_ptr< osg::Node > white_ducts;
    osg::ref_ptr< osg::Node > yellow;

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

    osg::ref_ptr< osg::Material > aluminum_pipes_material;
    osg::ref_ptr< osg::Material > aluminum_parts_material;
    osg::ref_ptr< osg::Material > black_material;
    osg::ref_ptr< osg::Material > brown_material;
    osg::ref_ptr< osg::Material > details_material;
    osg::ref_ptr< osg::Material > glass_material;
    osg::ref_ptr< osg::Material > lights_material;
    osg::ref_ptr< osg::Material > lt_green_material;
    osg::ref_ptr< osg::Material > lt_grey_material;
    osg::ref_ptr< osg::Material > orange_material;
    osg::ref_ptr< osg::Material > red_material;
    osg::ref_ptr< osg::Material > red_brown_material;
    osg::ref_ptr< osg::Material > ceiling_material;
    osg::ref_ptr< osg::Material > floor_material;
    osg::ref_ptr< osg::Material > walls_material;
    osg::ref_ptr< osg::Material > white_pipes_material;
    osg::ref_ptr< osg::Material > white_ducts_material;
    osg::ref_ptr< osg::Material > yellow_material;

    osg::ref_ptr< osg::Material > frame_material;
    osg::ref_ptr< osg::Material > railing_material;
    osg::ref_ptr< osg::Material > plenum_piping_material;
    osg::ref_ptr< osg::Material > blower_components_material;
    osg::ref_ptr< osg::Material > brackets_material;
    osg::ref_ptr< osg::Material > cement_base_material;
    osg::ref_ptr< osg::Material > combustor_piping_material;
    osg::ref_ptr< osg::Material > compressor_inlet_material;
    osg::ref_ptr< osg::Material > heat_exchanger_material;
    osg::ref_ptr< osg::Material > heat_exchanger_sweep_material;
    osg::ref_ptr< osg::Material > load_material;
    osg::ref_ptr< osg::Material > plenum_system_material;
    osg::ref_ptr< osg::Material > relief_piping_material;
    osg::ref_ptr< osg::Material > shell_material;
    osg::ref_ptr< osg::Material > stack_material;
    osg::ref_ptr< osg::Material > turbine_exhaust_material;
    osg::ref_ptr< osg::Material > turbine_postcombustor_material;
    osg::ref_ptr< osg::Material > miscellaneous_material;
};

#endif //SCENE_H
