#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include "cfdAdvectPropertyCallback.h"
#include "cfdCopyTo3DTextureStage.h"
#include <osgUtil/Cullvisitor>
#include <osg/Geode>
#include <osg/Geometry>
//////////////////////////////////////////////////////////////////////////
//Constructor                                                           //
//////////////////////////////////////////////////////////////////////////
cfdAdvectPropertyCallback::cfdAdvectPropertyCallback(osg::Node* subgraph,
                                                     int nSlices)
:_subgraph(subgraph)
{
   _nSlices = nSlices;
   _currentSlice = 0;
}
///////////////////////////////////////////////////////
//Destructor                                         //
///////////////////////////////////////////////////////
cfdAdvectPropertyCallback::~cfdAdvectPropertyCallback()
{
}
////////////////////////////////////////////////////////
unsigned int cfdAdvectPropertyCallback::GetCurrentSlice()
{
   return _currentSlice;
}
////////////////////////////////////////////////////////////////
void cfdAdvectPropertyCallback::operator()(osg::Node* node,
                                      osg::NodeVisitor* nv)
{
   cfdSliceNodeVisitor* myCullVisitor = dynamic_cast<cfdSliceNodeVisitor*>(nv);
   if (myCullVisitor && _subgraph.valid()){
      float delta = 1.0/(float)_nSlices;
      osg::Geode* geode = dynamic_cast<osg::Geode*>(_subgraph.get());
      osg::Geometry* geom = dynamic_cast<osg::Geometry*>(geode->getDrawable(0));
      //set up the current texture coords
      osg::Vec3Array* texcoords = 
            dynamic_cast<osg::Vec3Array*>(geom->getTexCoordArray(0));
         
      for(unsigned int i = 0; i < _nSlices; i++){
         (*texcoords)[0].set(0.0f,0.0f,i*delta); 
         (*texcoords)[1].set(1.0f,0.0f,i*delta); 
         (*texcoords)[2].set(1.0f,1.0f,i*delta); 
         (*texcoords)[3].set(0.0f,1.0f,i*delta);
         myCullVisitor->SetSliceNumber(i);

          _subgraph->accept(*nv);

         // must call this             
         traverse(node,nv);
      }    
   }
}  
#endif
#endif
