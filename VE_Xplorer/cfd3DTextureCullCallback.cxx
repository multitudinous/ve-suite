#ifdef _OSG

#ifdef CFD_USE_SHADERS
#include "cfd3DTextureCullCallback.h"
//#include "cfdCopyTo3DTextureStage.h"
#include "cfdPBufferManager.h"

#include <osg/Node>
#include <osg/NodeVisitor>
#include <osg/Texture3D>
#include <osgUtil/CullVisitor>
#include <osg/Viewport>
#include <osg/Geode>
#include <osg/Geometry>
#include <osgUtil/RenderStage>
//#include <osg/BoundingBox>


////////////////////////////////////////////////////////////////////////////
cfd3DTextureCullCallback::cfd3DTextureCullCallback(osg::Node* subgraph,
                                             unsigned int width,
                                             unsigned int height)
:_subgraph(subgraph)
{
   _w = width;
   _h = height;
}
/////////////////////////////////////////////////////
cfd3DTextureCullCallback::~cfd3DTextureCullCallback()
{
   
}
////////////////////////////////////////////////////////////
void cfd3DTextureCullCallback::operator()(osg::Node* node, 
                                     osg::NodeVisitor* nv)
{
   osgUtil::CullVisitor* cullVisitor = dynamic_cast<osgUtil::CullVisitor*>(nv); 
   if (cullVisitor && _subgraph.valid()){
      preRender(*node,*cullVisitor);
      // must traverse the Node's subgraph            
      traverse(node,nv);
   }

}
////////////////////////////////////////////////////////
void cfd3DTextureCullCallback::preRender(osg::Node& node,
                                    osgUtil::CullVisitor& cv)
{

   const osg::BoundingSphere& bs = _subgraph->getBound();
    if (!bs.valid())
    {
        osg::notify(osg::WARN) << "bb invalid"<<_subgraph.get()<<std::endl;
        return;
    }

    // create the render to texture stage.
    osg::ref_ptr<osgUtil::RenderStage> rtts = new osgUtil::RenderStage;
    
    // set up lighting.
    // currently ignore lights in the scene graph itself..
    // will do later.
    osgUtil::RenderStage* previous_stage = cv.getCurrentRenderBin()->getStage();

    // set up the background color and clear mask.
    rtts->setClearColor(osg::Vec4(0.0f,0.0,0.0f,1.0f));
    rtts->setClearMask(previous_stage->getClearMask());

    // set up to charge the same RenderStageLighting is the parent previous stage.
    rtts->setRenderStageLighting(previous_stage->getRenderStageLighting());

    // record the render bin, to be restored after creation
    // of the render to text
    osgUtil::RenderBin* previousRenderBin = cv.getCurrentRenderBin();

    // set the current renderbin to be the newly created stage.
    cv.setCurrentRenderBin(rtts.get());

    if (!_localState) _localState = new osg::StateSet;
    cv.pushStateSet(_localState.get());
    {
        // traverse the subgraph
        _subgraph->accept(cv);
    }
    cv.popStateSet();
    // restore the previous renderbin.
    cv.setCurrentRenderBin(previousRenderBin);

    if (rtts->getRenderGraphList().size()==0 && rtts->getRenderBinList().size()==0)
    {
        // getting to this point means that all the subgraph has been
        // culled by small feature culling or is beyond LOD ranges.
        return;
    }

    int height = _h;
    int width  = _w;

    
    osg::Viewport* new_viewport = new osg::Viewport;
    new_viewport->setViewport(0,0,width,height);
    rtts->setViewport(new_viewport);

    _localState->setAttribute(new_viewport);    

    // and the render to texture stage to the current stages
    // dependancy list.
    cv.getCurrentRenderBin()->getStage()->addToDependencyList(rtts.get());
}
   
#endif
#endif