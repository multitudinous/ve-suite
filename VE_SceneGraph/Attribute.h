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
 * File:          $RCSfile: cfdNode.h,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_SCENE_GRAPH_ATTRIBUTE_H
#define VE_SCENE_GRAPH_ATTRIBUTE_H

#include <string>


#ifdef _PERFORMER
#elif _OSG
#include <osg/StateSet>;
#include <osg/ref_ptr>
#elif _PERFORMER
#elif _OPENSG
#endif

namespace VE_SceneGraph
{
namespace Utilities
{
#ifdef _OSG
class VE_SCENEGRAPH_UTILS_EXPORTS Attribute: public osg::StateSet
#elif _PERFORMER
class VE_SCENEGRAPH_UTILS_EXPORTS Attribute: public osg::StateSet
#include <Performer/pr/pfMaterial.h>
#endif
{
public:   
   ///Constructor
   Attribute();
#ifdef _OSG
   ///Copy Constructor for OpenSceneGraph object
   Attribute(const Attribute& pbQuad,
             const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY);

   ///OSG defines this macro
   META_Object(VE_SceneGraph::Utilities,Attribute);
   
   osg::StateSet* GetStateSet(); 
#elif _PERFORMER
   pfGeoState* GetStateSet(); 
#endif
   

   ///Destructor
   virtual ~Attribute();

   
protected:

};
}
}
#endif //VE_SCENE_GRAPH_ATTRIBUTE_H
