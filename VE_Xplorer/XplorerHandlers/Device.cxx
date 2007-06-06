/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Stuff --- //
#include "VE_Xplorer/XplorerHandlers/Device.h"

#include "VE_Xplorer/SceneGraph/SceneManager.h"

// --- OSG Stuff --- //
#include <osg/LineSegment>
#include <osg/Material>

#include <osgUtil/IntersectVisitor>

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
Device::Device()
:
//device_mode( 0 ),
center_point( 0 ),
m_threshold( 0 ),
m_jump( 0 )
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
void Device::SetVECommand( VE_XML::Command* command )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Device::UpdateCommand()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool Device::CheckCommandId( VE_Xplorer::cfdCommandArray * _cfdCommandArray )
{
    return false;
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* Device::GetActiveDCS()
{
    return activeDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetActiveDCS( VE_SceneGraph::DCS* dcs )
{
   activeDCS = dcs;
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetCenterPoint( gmtl::Point3d* cp )
{
   center_point = cp;
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetCenterPointThreshold( float* threshold )
{
    m_threshold = threshold;
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetCenterPointJump( float* jump )
{
    m_jump = jump;
}
////////////////////////////////////////////////////////////////////////////////
void Device::ProcessSelection()
{
   osg::Vec3d start_point;
   osg::Vec3d end_point;
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
   VE_SceneGraph::SceneManager::instance()->GetRootNode()->accept( intersect_visitor );

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

   this->DrawLine( start_point, end_point );
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetStartEndPoint( osg::Vec3d* startPoint, osg::Vec3d* endPoint )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Device::DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
