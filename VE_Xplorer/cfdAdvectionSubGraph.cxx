#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include "cfdAdvectionSubGraph.h"
#include <osg/Group>
#include <osg/Geode>
#include <osg/BoundingBox>
#include <osg/Projection>
#include <osg/MatrixTransform>
#include <osg/Geometry>

osg::Node* CreateAdvectionSubGraph(unsigned int w,unsigned int h)
{
   osg::ref_ptr<osg::Geode> geode = new osg::Geode();
   osg::ref_ptr<osg::StateSet> stateset = geode->getOrCreateStateSet();
    stateset->setMode(GL_LIGHTING,osg::StateAttribute::ON);
    //stateset->setMode(GL_DEPTH_TEST,osg::StateAttribute::OFF);
    stateset->setRenderBinDetails(11,"RenderBin");
   
    osg::BoundingBox bbox;
    bbox.set(osg::Vec3(-1,-1,0),osg::Vec3(1,1,0));


    osg::ref_ptr<osg::Geometry> geom = new osg::Geometry;

    osg::Vec3Array* vertices = new osg::Vec3Array;
    osg::Vec3Array* texCoords = new osg::Vec3Array;
    
    float depth = bbox.zMin()-0.1;
    texCoords->push_back(osg::Vec3(0,1,depth));
    texCoords->push_back(osg::Vec3(0,0,depth));
    texCoords->push_back(osg::Vec3(1,0,depth));
    texCoords->push_back(osg::Vec3(1,1,depth));
    geom->setTexCoordArray(0,texCoords);

    depth = bbox.zMin()-0.1;
    vertices->push_back(osg::Vec3(bbox.xMin(),bbox.yMax(),depth));
    vertices->push_back(osg::Vec3(bbox.xMin(),bbox.yMin(),depth));
    vertices->push_back(osg::Vec3(bbox.xMax(),bbox.yMin(),depth));
    vertices->push_back(osg::Vec3(bbox.xMax(),bbox.yMax(),depth));
    geom->setVertexArray(vertices);

    osg::Vec3Array* normals = new osg::Vec3Array;
    normals->push_back(osg::Vec3(0.0f,0.0f,1.0f));
    geom->setNormalArray(normals);
    geom->setNormalBinding(osg::Geometry::BIND_OVERALL);

    osg::Vec4Array* colors = new osg::Vec4Array;
    colors->push_back(osg::Vec4(1.0f,1.0,1.0f,1.0f));
    geom->setColorArray(colors);
    geom->setColorBinding(osg::Geometry::BIND_OVERALL);

    geom->addPrimitiveSet(new osg::DrawArrays(GL_QUADS,0,4));
        
    //osg::ref_ptr<osg::StateSet> geomSS = geom->getOrCreateStateSet();
    //may need to add these back
    //geomSS->setMode(GL_BLEND,osg::StateAttribute::ON);
    //geomSS->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
    geode->addDrawable(geom.get());

    // create the hud.
    osg::ref_ptr<osg::MatrixTransform> modelview_abs = new osg::MatrixTransform;
    modelview_abs->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
    modelview_abs->setMatrix(osg::Matrix::identity());
    modelview_abs->addChild(geode.get());

    osg::ref_ptr<osg::Projection> projection = new osg::Projection;
    projection->setMatrix(osg::Matrix::ortho2D(0,w,0,h));
    projection->addChild(modelview_abs.get());

    return projection.get();
   
}
#endif// CFD_USE_SHADERS
#endif// _OSG
