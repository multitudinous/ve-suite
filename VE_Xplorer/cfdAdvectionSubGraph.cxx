#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include "cfdAdvectionSubGraph.h"
#include "cfdTextureManager.h"
#include <osg/Group>
#include <osg/Geode>
#include <osg/BoundingBox>
#include <osg/Projection>
#include <osg/MatrixTransform>
#include <osg/Geometry>
#include <osg/TexGenNode>
osg::ref_ptr<osg::TexGenNode> CreateAdvectionSubGraph(cfdTextureManager* tm)//unsigned int w,unsigned int h)
{
   if(!tm) return 0;
   osg::ref_ptr<osg::Geode> geode = new osg::Geode();
   osg::ref_ptr<osg::StateSet> stateset = geode->getOrCreateStateSet();
    stateset->setMode(GL_LIGHTING,osg::StateAttribute::ON);
    //stateset->setMode(GL_DEPTH_TEST,osg::StateAttribute::OFF);
    stateset->setRenderBinDetails(11,"RenderBin");
   
    /*osg::BoundingBox bbox;
    bbox.set(osg::Vec3(-1,-1,0),osg::Vec3(1,1,0));


    
    osg::Vec3Array* texCoords = new osg::Vec3Array;
    
     depth = bbox.zMin()-0.1;
    texCoords->push_back(osg::Vec3(0,1,depth));
    texCoords->push_back(osg::Vec3(0,0,depth));
    texCoords->push_back(osg::Vec3(1,0,depth));
    texCoords->push_back(osg::Vec3(1,1,depth));
    geom->setTexCoordArray(0,texCoords);
    */
    osg::ref_ptr<osg::Geometry> geom = new osg::Geometry;

    osg::Vec3Array* vertices = new osg::Vec3Array;
    float depth = -0.1;
    vertices->push_back(osg::Vec3(-1,1,depth));
    vertices->push_back(osg::Vec3(-1,-1,depth));
    vertices->push_back(osg::Vec3(1,-1,depth));
    vertices->push_back(osg::Vec3(1,1,depth));
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

    float* BBOX = tm->getBoundingBox();
    osg::BoundingBox bbox;
    float minBBox[3];
    float maxBBox[3];
    //this is because vtk gives mnx,mxx,mny,mxy,mnz,mxz
    minBBox[0] = BBOX[0]; 
    minBBox[1] = BBOX[2]; 
    minBBox[2] = BBOX[4]; 
    maxBBox[0] = BBOX[1]; 
    maxBBox[1] = BBOX[3]; 
    maxBBox[2] = BBOX[5]; 
    bbox.set(osg::Vec3(minBBox[0],minBBox[1],minBBox[2]), 
                osg::Vec3(maxBBox[0],maxBBox[1],maxBBox[2]));
    osg::Vec4 sPlane(0,0,0,0);
    osg::Vec4 tPlane(0,0,0,0);
    osg::Vec4 rPlane(0,0,0,0);

    sPlane[0] = 1.0/(bbox.xMax() - bbox.xMin());
    tPlane[1] = 1.0/(bbox.yMax() - bbox.yMin());
    rPlane[2] = 1.0/(bbox.zMax()- bbox.zMin());
   
    sPlane[3] = - bbox.xMin()/(bbox.xMax() - bbox.xMin());
    tPlane[3] = - bbox.yMin()/(bbox.yMax() - bbox.yMin());
    rPlane[3] = - bbox.zMin()/(bbox.zMax()- bbox.zMin());

    //biv--this may not be right!!!      
    osg::ref_ptr<osg::TexGenNode> texGenParams = new osg::TexGenNode();
    texGenParams->setTextureUnit(0);
    texGenParams->getTexGen()->setMode(osg::TexGen::OBJECT_LINEAR);
    texGenParams->getTexGen()->setPlane(osg::TexGen::S,sPlane); 
    texGenParams->getTexGen()->setPlane(osg::TexGen::T,tPlane);
    texGenParams->getTexGen()->setPlane(osg::TexGen::R,rPlane);

    texGenParams->addChild(geode.get());
   

    return texGenParams.get();
   
}
#endif// CFD_USE_SHADERS
#endif// _OSG
