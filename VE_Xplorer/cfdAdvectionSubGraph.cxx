#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include <cmath>
#include "cfdAdvectionSubGraph.h"
#include "cfdTextureManager.h"
#include "cfdPBufferManager.h"
#include "cfdVolumeVisualization.h"
#include "cfdTextureMatrixCallback.h"

#include <osg/Group>
#include <osg/Geode>
#include <osg/BoundingBox>
#include <osg/Projection>
#include <osg/MatrixTransform>
#include <osg/Geometry>
#include <osg/Texture3D>
#include <osg/TexMat>


osg::ref_ptr<osg::TexGenNode> CreateAdvectionSubGraph(cfdTextureManager* tm,
                                                osg::Texture3D* updateTexture,
                                                cfdPBufferManager* pbm,
                                                osg::StateSet* stateset,
                                                float deltaZ)
{
   if(!tm) 
      return 0;
   
   osg::ref_ptr<osg::Geode> geode = new osg::Geode();
   geode->setStateSet(stateset);
   osg::ref_ptr<osg::Geometry> geom = new osg::Geometry;

   osg::Vec3Array* normals = new osg::Vec3Array;
   normals->push_back(osg::Vec3(0.0f,0.0f,1.0f));

   geom->setNormalArray(normals);
   geom->setNormalBinding(osg::Geometry::BIND_OVERALL);

   osg::Vec4Array* colors = new osg::Vec4Array;
   colors->push_back(osg::Vec4(1.0f,1.0,1.0f,1.0f));

   geom->setColorArray(colors);
   geom->setColorBinding(osg::Geometry::BIND_OVERALL);

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

   float radius = bbox.radius();
   float center[3] = {0,0,0};

   center[0] = bbox.center()[0];
   center[1] = bbox.center()[1];
   center[2] = bbox.center()[2];
   float scale[3] = {1,1,1};

   scale[0] = fabs(maxBBox[0] - minBBox[0]);
   scale[1] = fabs(maxBBox[1] - minBBox[1]);
   scale[2] = fabs(maxBBox[2] - minBBox[2]);

   scale[0] = 1.0/scale[0];
   scale[1] = 1.0/scale[1];
   scale[2] = 1.0/scale[2];

   //recalculate the bbox that is going to be used for
   //the volume vis
   minBBox[0] = center[0] - radius;
   minBBox[1] = center[1] - radius;
   minBBox[2] = center[2] - radius;

   maxBBox[0] = center[0] + radius;
   maxBBox[1] = center[1] + radius;
   maxBBox[2] = center[2] + radius;

   unsigned int nSlices = tm->fieldResolution()[2];
   //reset the bbox
   bbox.set(osg::Vec3(minBBox[0],minBBox[1],minBBox[2]), 
                osg::Vec3(maxBBox[0],maxBBox[1],maxBBox[2]));

   osg::Vec3Array* zcoords = new osg::Vec3Array(4*nSlices);
  
   float z = bbox.zMin();
 
   float dz = (bbox.zMax() - bbox.zMin())/(nSlices - 1.0);
   for(unsigned int i = 0; i < nSlices; i++, z+=dz){
      (*zcoords)[i*4+0].set(bbox.xMax(),bbox.yMin(),z);
      (*zcoords)[i*4+1].set(bbox.xMax(),bbox.yMax(),z);
      (*zcoords)[i*4+2].set(bbox.xMin(),bbox.yMax(),z);
      (*zcoords)[i*4+3].set(bbox.xMin(),bbox.yMin(),z);
   }

   geom->setVertexArray(zcoords);
   geom->addPrimitiveSet(new osg::DrawArrays(GL_QUADS,0,zcoords->size()));

   //set up the draw callback
   osg::ref_ptr<pbufferDrawCallback> drawCallback = 
       new pbufferDrawCallback(updateTexture,
                             pbm,
                             tm->fieldResolution()[2],
                             center,
                             radius);

   drawCallback->SetDimensions(tm->fieldResolution()[0],tm->fieldResolution()[1]);

   geom->setDrawCallback(drawCallback.get());
   geom->setUseDisplayList(false);
   geode->addDrawable(geom.get());

   //texture generation equations
   osg::Vec4 sPlane(1,0,0,0);
   osg::Vec4 tPlane(0,1,0,0);
   osg::Vec4 rPlane(0,0,1,0);
     
   osg::ref_ptr<osg::TexGenNode> texGenParams = new osg::TexGenNode();
   texGenParams->setTextureUnit(0);
   texGenParams->getTexGen()->setMode(osg::TexGen::EYE_LINEAR);
   texGenParams->getTexGen()->setPlane(osg::TexGen::S,sPlane); 
   texGenParams->getTexGen()->setPlane(osg::TexGen::T,tPlane);
   texGenParams->getTexGen()->setPlane(osg::TexGen::R,rPlane);

   osg::ref_ptr<osg::TexMat> tMat = new osg::TexMat();
   tMat->setMatrix(osg::Matrix::identity());
   stateset->setTextureAttributeAndModes(0,tMat.get());

   float trans[3] = {.5,.5,.5};
   
   texGenParams->setUpdateCallback(new cfdTextureMatrixCallback(tMat.get(),
                                 osg::Vec3(center[0],center[1],center[2]),
                                                      scale,trans));
   texGenParams->addChild(geode.get());
   return texGenParams.get();
   
}
///////////////////////////////////////////////////////////////////////
pbufferDrawCallback::pbufferDrawCallback(osg::Texture3D* updateTexture,
                                    cfdPBufferManager* pbm,
                                    unsigned int nSlices,
                                    float* center,
                                    float radius)
{
   _nSlices = nSlices;
   _texture = updateTexture;
   _pbuffer = pbm;
   _radius = radius;
   _center[0] = center[0];
   _center[1] = center[1];
   _center[2] = center[2];

   if(_pbuffer){
      float lookFrom[3] = {0,0,10};
      lookFrom[0] = _center[0];
      lookFrom[1] = _center[1];
      lookFrom[2] = _center[2] + _radius + 10;

      float up[3] = {0,1,0};
      
      _pbuffer->setLookAt(_center);
      _pbuffer->setLookFrom(lookFrom);
      _pbuffer->setLookUp(up);
      _pbuffer->setCenterOfInterest(_center[2]);
      _pbuffer->setGeometricParams(_center,_radius);
   }
   _w = 64;
   _h = 64;
}
///////////////////////////////////////////
pbufferDrawCallback::~pbufferDrawCallback()
{
}
/////////////////////////////////////////////////////////////////
void pbufferDrawCallback::SetDimensions(unsigned int w,unsigned int h)
{
   _w = w;
   _h = h;
}
///////////////////////////////////////////////////////////////
void pbufferDrawCallback::drawImplementation(osg::State& state,
                                       const osg::Drawable* drawable) const
{
   if(_pbuffer){
      if(!_pbuffer->isCreated()){
         _pbuffer->initializePBuffer(_w,_h);
      }
      _pbuffer->beginSlicing(_nSlices);

      if(_pbuffer->activate()){
         
         const unsigned int contextID = state.getContextID();
         osg::Texture::TextureObject* textureObject =
                                   _texture->getTextureObject(contextID);
         if (textureObject == 0){
            // Make sure texture is loaded, subload callback required.
            _texture->apply(state);
         }
         osg::ref_ptr<osg::Texture3D> tempTexture = _texture;
         for(unsigned int i = 0; i < _nSlices; i++){
            _pbuffer->initSlice(i);
            _pbuffer->applyProjectionMatrix();
            _pbuffer->applyViewMatrix();
            drawable->drawImplementation(state);
            //copy into our texture
            //make this more general later 
            tempTexture->copyTexSubImage3D(state,
                                     0,0,i,
                                     0,0,_w,_h);
         }
         _pbuffer->endSlicing();
         _pbuffer->deactivate();   
      }
   }
}
#endif// CFD_USE_SHADERS
#endif// _OSG
