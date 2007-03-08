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
#ifndef MODEL_OCCLUDER
#define MODEL_OCCLUDER
/*!\file ModelOccluder.h
ModelOccluder API
*/

/*!\class VE_SceneGraph::ModelOccluder
*
*/

#include "VE_Installer/include/VEConfig.h"
#include <string>

#ifdef _PERFORMER
#elif _OSG
#include <osg/ref_ptr>
#include <osg/Vec3>
namespace osg
{
   class Node;
   class Group;
   //class Vec3;
   class OccluderNode;
}
namespace VE_SceneGraph
{
   class cfdNode;
}
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
   
   osg::ref_ptr<osg::Group> GetOccluderNode( osg::Node* node );
   osg::ref_ptr<osg::OccluderNode> createOccluder(const osg::Vec3& v1,const osg::Vec3& v2,const osg::Vec3& v3,const osg::Vec3& v4,float holeRatio=-1.0f);
   osg::ref_ptr<osg::Group> createOccludersAroundModel(osg::Node* model);
#elif _OPENSG
#endif
protected:
#ifdef _PERFORMER
#elif _OSG
   osg::ref_ptr<osg::Node> modelNode;
#elif _OPENSG
#endif
};
}
#endif
