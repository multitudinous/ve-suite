#ifdef VE_PATENTED
#ifdef _OSG
#include <cmath>
#include "VE_TextureBased/cfdAdvectionSubGraph.h"
#include "VE_TextureBased/cfdOSGAdvectionShaderManager.h"
#include "VE_TextureBased/cfdTextureManager.h"
#include "VE_TextureBased/cfdPBufferManager.h"
#include "VE_TextureBased/cfdVolumeVisualization.h"
#include "VE_TextureBased/cfdTextureMatrixCallback.h"
#include "VE_TextureBased/cfdPBufferQuad.h"

#include <osg/Group>
#include <osg/Geode>
#include <osg/BoundingBox>
#include <osg/Projection>
#include <osg/MatrixTransform>
#include <osg/Geometry>
#include <osg/Texture3D>
#include <osg/TexMat>
#include <osg/TexEnv>
#include <osg/StateSet>
#include <osg/TexGenNode>

using namespace VE_TextureBased;

osg::ref_ptr<osg::Group> VE_TextureBased::CreateAdvectionSubGraph(cfdTextureManager* tm,
                                            cfdPBufferManager* pbm,
                                            float deltaZ)
{
   if(!tm) 
      return 0;
   
   osg::ref_ptr<osg::Geode> geode = new osg::Geode();

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

   osg::ref_ptr<cfdPBufferQuad> pbSlice = new cfdPBufferQuad;
   pbSlice->SetBBox(BBOX);
   pbSlice->SetNumberOfSlices(tm->fieldResolution()[2]);
   pbSlice->SetUseAutoTexCoords(false);
   pbSlice->SetTextureDimensions(tm->fieldResolution()[0],
   tm->fieldResolution()[1],
   tm->fieldResolution()[2]);

   //pbSlice->SetTextureToUpdate(updateTexture);
   pbSlice->CalculateSlices();
   pbSlice->setUseDisplayList(false);
   geode->addDrawable(pbSlice.get());
   osg::ref_ptr<osg::Group> group = new osg::Group;
   group->setName("Advection Slice");
   group->addChild(geode.get());
   return group;
}
#endif// _OSG
#endif
