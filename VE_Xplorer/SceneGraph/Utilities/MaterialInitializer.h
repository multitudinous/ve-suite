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
 * Date modified: $Date: 2007-03-23 12:23:48 -0500 (Fri, 23 Mar 2007) $
 * Version:       $Rev: 7205 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: MaterialInitializer.h 7205 2007-03-23 17:23:48Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef MATERIAL_INITIALIZER_H
#define MATERIAL_INITIALIZER_H
/*!\file MaterialInitializer.h
MaterialInitializer API
*/
/*!\class MaterialInitializer
* 
*/
#include "VE_Installer/include/VEConfig.h"

#include <osg/ref_ptr>
#include <osg/NodeVisitor>


namespace VE_SceneGraph
{
namespace Utilities
{
class VE_SCENEGRAPH_UTILS_EXPORTS MaterialInitializer : public osg::NodeVisitor
{
public:
	MaterialInitializer( osg::Node* osg_node );
	virtual ~MaterialInitializer();
        
   virtual void apply( osg::Geode& node );                     //{ apply((Node&)node); }
   //virtual void apply( osg::Billboard& node );                 //{ apply((Geode&)node); }
        
   //virtual void apply( osg::Group& node );                     //{ apply((Node&)node); }

   //virtual void apply( osg::ProxyNode& node );                 //{ apply((Group&)node); }

   //virtual void apply( osg::Projection& node );                //{ apply((Group&)node); }

   //virtual void apply( osg::CoordinateSystemNode& node );      //{ apply((Group&)node); }

   //virtual void apply( osg::ClipNode& node );                  //{ apply((Group&)node); }
   //virtual void apply( osg::TexGenNode& node );                //{ apply((Group&)node); }
   //virtual void apply( osg::LightSource& node );               //{ apply((Group&)node); }

   //virtual void apply( osg::Transform& node );                 //{ apply((Group&)node); }
   //virtual void apply( osg::CameraNode& node );                //{ apply((Transform&)node); }
   //virtual void apply( osg::CameraView& node );                //{ apply((Transform&)node); }
   //virtual void apply( osg::MatrixTransform& node );           //{ apply((Transform&)node); }
   //virtual void apply( osg::PositionAttitudeTransform& node ); //{ apply((Transform&)node); }

   //virtual void apply( osg::Switch& node );                    //{ apply((Group&)node); }
   //virtual void apply( osg::Sequence& node );                  //{ apply((Group&)node); }
   //virtual void apply( osg::LOD& node );                       //{ apply((Group&)node); }
   //virtual void apply( osg::PagedLOD& node );                  //{ apply((LOD&)node); }
   //virtual void apply( osg::ClearNode& node );                 //{ apply((Group&)node); }
   //virtual void apply( osg::OccluderNode& node );              //{ apply((Group&)node); }

private:

};
}
}

#endif //MATERIAL_INITIALIZER_H
