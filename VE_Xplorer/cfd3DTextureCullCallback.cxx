#ifdef _OSG
#include <osg/Geode>
#include <osg/Geometry>
#ifdef CFD_USE_SHADERS
#include "cfdAdvectPropertyCallback.h"
#include "cfd3DTextureCullCallback.h"
#include "cfdCopyTo3DTextureStage.h"
////////////////////////////////////////////////////////////////////////////
cfd3DTextureCullCallback::cfd3DTextureCullCallback(osg::Node* subgraph,
		                                       osg::Texture3D* texture,
						                            cfdPBufferManager* pbm,
                                            unsigned int nSlices)
:_subgraph(subgraph),_textureToUpdate(texture) 
{
   _pbuffer = pbm;               
   _nSlices = nSlices;
}
////////////////////////////////////////////////////////////
void cfd3DTextureCullCallback::operator()(osg::Node* node, 
                                     osg::NodeVisitor* nv)
{
    /*float delta = 1.0/(float)_nSlices;
         osg::ref_ptr<osg::Geode> geode = 
            dynamic_cast<osg::Geode*>(((osg::Group*)_subgraph.get())->getChild(0));
            
         osg::Geometry* geom = dynamic_cast<osg::Geometry*>(geode->getDrawable(0));
         //set up the current texture coords
         osg::Vec3Array* texcoords = 
            dynamic_cast<osg::Vec3Array*>(geom->getTexCoordArray(0));
      
         (*texcoords)[0].set(0.0f,0.0f,i*delta); 
         (*texcoords)[1].set(1.0f,0.0f,i*delta); 
         (*texcoords)[2].set(1.0f,1.0f,i*delta); 
         (*texcoords)[3].set(0.0f,1.0f,i*delta);*/
   osgUtil::CullVisitor* cullVisitor = dynamic_cast<osgUtil::CullVisitor*>(nv); 
   if (cullVisitor && _textureToUpdate.valid()&& _subgraph.valid()){
      unsigned int sliceNumber = 0;
      for(unsigned int i = 0; i < _nSlices; i++){
         preRender(*node,*cullVisitor, i);
      }
      // must traverse the Node's subgraph            
      traverse(node,nv);
   }

}
////////////////////////////////////////////////////////
void cfd3DTextureCullCallback::preRender(osg::Node& node,
                                    osgUtil::CullVisitor& cv,
					                       int sliceNumber)
{
   cv.getState()->setReportGLErrors(true);
   const osg::BoundingSphere& bs = _subgraph->getBound();
   if(!bs.valid()){
      osg::notify(osg::WARN) << "bb invalid"<<_subgraph.get()<<std::endl;
      return;
   } 

   int height = 0;
   int width = 0;
   int depth = 0;

   _textureToUpdate->getTextureSize(width,height,depth);

   // create the copy to 3dtexture stage.
   osg::ref_ptr<cfdCopyTo3DTextureStage> update3DTexture = new cfdCopyTo3DTextureStage;
    
    //init the pbuffer if needed
    if(!_pbuffer->isCreated()){
       _pbuffer->initializePBuffer(width,height);
    }
    update3DTexture->setPBuffer(_pbuffer);
    update3DTexture->setSliceToUpdate(sliceNumber);
 
    // set up lighting.
    // currently ignore lights in the scene graph itself..
    // will do later.
    osgUtil::RenderStage* previous_stage = cv.getCurrentRenderBin()->getStage();

    // set up the background color and clear mask.
    update3DTexture->setClearColor(osg::Vec4(0.0f,0.0f,0.0f,1.0f));
    update3DTexture->setClearMask(previous_stage->getClearMask());

    // set up to change the same RenderStageLighting as the parent previous stage.
    update3DTexture->setRenderStageLighting(previous_stage->getRenderStageLighting());

    // record the render bin, to be restored after creation
    // of the render to text
    osgUtil::RenderBin* previousRenderBin = cv.getCurrentRenderBin();

    // set the current renderbin to be the newly created stage.
    cv.setCurrentRenderBin(update3DTexture.get());

    float znear = 1.0f*bs.radius();
    float zfar  = 3.0f*bs.radius();
        
    // 2:1 aspect ratio as per flag geomtry below.
    float top   = 0.25f*znear;
    float right = 0.5f*znear;

    znear *= 0.9f;
    zfar *= 1.1f;

    // set up projection.
    osg::RefMatrix* projection = new osg::RefMatrix;
    projection->makeFrustum(-right,right,-top,top,znear,zfar);

    cv.pushProjectionMatrix(projection);

    osg::RefMatrix* matrix = new osg::RefMatrix;
    matrix->makeLookAt(bs.center()+osg::Vec3(0.0f,2.0f,0.0f)*bs.radius(),bs.center(),osg::Vec3(0.0f,0.0f,1.0f));

    cv.pushModelViewMatrix(matrix);

    if (!_localState.valid()){
       _localState = new osg::StateSet;
    }
    cv.pushStateSet(_localState.get());

    {
       // traverse the subgraph
       _subgraph->accept(cv);
    }

    cv.popStateSet();

    // restore the previous model view matrix.
    cv.popModelViewMatrix();

    // restore the previous model view matrix.
    cv.popProjectionMatrix();

    // restore the previous renderbin.
    cv.setCurrentRenderBin(previousRenderBin);

    if (update3DTexture->getRenderGraphList().size()==0 
       && update3DTexture->getRenderBinList().size()==0)
    {
        // getting to this point means that all the subgraph has been
        // culled by small feature culling or is beyond LOD ranges.
        return;
    }
    // offset the impostor viewport from the center of the main window
    // viewport as often the edges of the viewport might be obscured by
    // other windows, which can cause image/reading writing problems.

    osg::Viewport* new_viewport = new osg::Viewport;
    new_viewport->setViewport(0,0,width,height);
    update3DTexture->setViewport(new_viewport);

    _localState->setAttribute(new_viewport);    

   // and the render to texture stage to the current stages
   // dependancy list.
   cv.getCurrentRenderBin()->getStage()->addToDependencyList(update3DTexture.get());

   // if one exist attach texture to the RenderToTextureStage.
   if(_textureToUpdate.valid()){
      update3DTexture->set3DTexture(_textureToUpdate.get());
   }
}
   
#endif
#endif