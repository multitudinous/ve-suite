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
 * Date modified: $Date: 2006-11-28 14:11:49 -0600 (Tue, 28 Nov 2006) $
 * Version:       $Rev: 6082 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef MODEL_OCCLUDER
#define MODEL_OCCLUDER
/*!\file ModelOccluder.h
ModelOccluder API
*/

/*!\class VE_SceneGraph::ModelOccluder
*
*/

//#include "VE_Xplorer/SceneGraph/cfdSceneNode.h"
#include <string>

#ifdef _PERFORMER
#elif _OSG
#include <osg/ref_ptr>
#elif _OPENSG
#endif

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS ModelOccluder//: public cfdSceneNode
{
public:   
   ModelOccluder( void );
   //copy constructor
   ModelOccluder( const ModelOccluder& );
   virtual ~ModelOccluder( void );

   //equal operator
   ModelOccluder& operator= ( const ModelOccluder& );

   ///Set the name of the node
   void SetModelNode(cfdNode* modelNode);

#ifdef _PERFORMER
   void pfTravNodeMaterial( pfNode* );
   void pfTravNodeFog( pfNode* node_1, pfFog* fog );
#elif _OSG
   void TravNodeOccluder(osg::Node*);
#elif _OPENSG
#endif
protected:
#ifdef _PERFORMER
#elif _OSG
   osg::ref_ptr<osg::Node> modelNode;
#elif _OPENSG
};
}
#endif
