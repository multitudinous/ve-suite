#include "VE_Xplorer/XplorerHandlers/Device.h"

#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"

// --- OSG Stuff --- //
#include <osg/LineSegment>
#include <osg/Material>

#include <osgUtil/IntersectVisitor>

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
Device::Device()
{
	;
}
////////////////////////////////////////////////////////////////////////////////
void Device::UpdateNavigation()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void Device::UpdateSelection()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetActiveDCS( VE_SceneGraph::DCS* dcs )
{
   activeDCS = dcs;
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* Device::GetActiveDCS( void )
{
   return activeDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
void Device::ProcessSelection()
{
   osg::Vec3f start_point;
   osg::Vec3f end_point;
   this->SetStartEndPoint( &start_point, &end_point );

   /*
   std::cout << start_point.x() << std::endl;
   std::cout << start_point.y() << std::endl;
   std::cout << start_point.z() << std::endl;
   std::cout << std::endl;
   */
   
   osg::ref_ptr< osg::LineSegment > line_segment = new osg::LineSegment();
   line_segment->set( start_point, end_point );

   osgUtil::IntersectVisitor intersect_visitor;
   intersect_visitor.addLineSegment( line_segment.get() );

   //Add IntersectVisitor to RootNode so that all geometry is checked and no transforms are applied to LineSegment
   VE_SceneGraph::cfdPfSceneManagement::instance()->GetRootNode()->accept( intersect_visitor );

   osgUtil::IntersectVisitor::HitList hit_list;
   hit_list = intersect_visitor.getHitList( line_segment.get() );

   //Traversal part
   osgUtil::Hit objectHit;

   osg::ref_ptr< osg::Geode > selected_geometry;

   if( hit_list.empty())
   {
      //return;
   }
   else
   {
      for( unsigned int i = 0; i < hit_list.size(); i++ )
      {
         objectHit = hit_list[i];
         /*
         if( objectHit._geode->getName() != this->laserName )
         {
            break;
         }
         */
      }
   
      if (objectHit._geode.valid())
      {
         if (!objectHit._geode->getName().empty())
         {
            if ( /*objectHit._geode->getName() != this->laserName
                  && */objectHit._geode->getName() != "Root Node") 
            {
               selected_geometry = objectHit._geode;
               std::cout << objectHit._geode->getName() << std::endl;
            }
         }

         else
         {
            selected_geometry = objectHit._geode;
            std::cout << objectHit._geode->getParents().front()->getName() << std::endl;
         }
      } 
   }

   osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet;
   osg::ref_ptr< osg::Material > material = new osg::Material;
   material->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.0f, 1.0f, 0.0f, 1.0f ) );
   stateset->setAttributeAndModes( material.get(), osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE );
   //selected_geometry->setStateSet( stateset.get() );

   this->DrawLine( start_point, end_point );
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetStartEndPoint( osg::Vec3f* startPoint, osg::Vec3f* endPoint )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void Device::DrawLine( osg::Vec3f startPoint, osg::Vec3f endPoint )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////