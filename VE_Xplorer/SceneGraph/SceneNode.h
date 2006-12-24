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
#ifndef SCENENODE_H
#define SCENENODE_H
/*!\file SceneNode.h
This is the base calss for all scenegraph nodes so that they
can be passed around vesuite generically
*/

/*!\class VE_SceneGraph::SceneNode
*
*/

#include "VE_Installer/include/VEConfig.h"
namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS SceneNode
{
public:
   enum cfdNodeType
   {
      CFD_NODE,
      CFD_GROUP,
      CFD_DCS,
      CFD_GEODE,
      CFD_SEQUENCE,
      CFD_SWITCH,
      CFD_OTHER
   };

   SceneNode();
   SceneNode(cfdNodeType nt);
   SceneNode( const SceneNode& );

   virtual ~SceneNode( void );
   SceneNode& operator=( const SceneNode& );
   //get the internal cfd node type
   virtual cfdNodeType GetCFDNodeType(){return _nt;}
   //set internal cfd node type
   virtual void SetCFDNodeType(cfdNodeType nt){_nt = nt;}

protected:
   //our scene graph specific nodes
   int _numParents;
   cfdNodeType _nt;
};
}
#endif
