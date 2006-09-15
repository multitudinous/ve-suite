#ifdef _OSG

#include <osg/Group>
#include <osg/LineSegment>
#include <osgUtil/IntersectVisitor>

#include "VE_Xplorer/XplorerHandlers/SelectionHandler.h"
#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"
#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"
#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Xplorer/SceneGraph/cfdGroup.h"

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
SelectionHandler::SelectionHandler()
{
   //selectedGeometry=0;
   active=false;
}
////////////////////////////////////////////////////////////////////////////////
SelectionHandler::~SelectionHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void SelectionHandler::ActivateSelection()
{
   active=true;
}
////////////////////////////////////////////////////////////////////////////////
void SelectionHandler::DeactivateSelection()
{
   active=false;
}
////////////////////////////////////////////////////////////////////////////////
void SelectionHandler::SelectObjects()
{
   osgUtil::IntersectVisitor intersectVisitor;
   osgUtil::IntersectVisitor::HitList hitList;
   osgUtil::Hit hit;
  
   //rootNode->accept(intersectVisitor);

   intersectVisitor.addLineSegment(CreateLineSegment().get());
   
   hitList=intersectVisitor.getHitList(CreateLineSegment().get());

   //selectedGeometry=NULL;

   if(hitList.empty()){
      vprDEBUG(vesDBG,1)<<"|\tcfdObjectHandler::ProcessHit No object selected"<<std::endl<<vprDEBUG_FLUSH;
   }
   else{
      for(unsigned int i=0;i<hitList.size();i++){
         //hit=hitList->at(i);
         //if(hit._geode->getName()!=laserName){

            //break;
         //}
      }
   
      if(hit._geode.valid()){
         if(!hit._geode->getName().empty()){
            //if(hit._geode->getName()!=laserName&&hit._geode->getName()!="Root Node"){
               //selectedGeometry=hit._geode;
               //std::cout<<hit._geode->getName()<<std::endl;
            //}
         }
         else{
            //this->selectedGeometry=hit->_geode;
            //std::cout<<hit->_geode->getParents().front()->getName()<<std::endl;
         }
      }
   }
}
////////////////////////////////////////////////////////////////////////////////

#endif
