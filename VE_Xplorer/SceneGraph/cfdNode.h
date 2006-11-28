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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_NODE_H
#define CFD_NODE_H
/*!\file cfdNode.h
cfdNode API
*/

/*!\class VE_SceneGraph::cfdNode
*
*/

#include "VE_Xplorer/SceneGraph/cfdSceneNode.h"
#include <string>


#ifdef _PERFORMER
class pfNode;
class pfLightModel;
class pfFog;
#elif _OSG
namespace osg 
{ 
   class Fog; 
   class Node;
   class LightModel;
}
#include <osg/ref_ptr>
#elif _OPENSG
#endif

namespace VE_SceneGraph
{
   class VE_SCENEGRAPH_EXPORTS cfdNode: public cfdSceneNode
   {
      public:   
         cfdNode( void );
         //copy constructor
         cfdNode( const cfdNode& );
         virtual ~cfdNode( void );

         //equal operator
         cfdNode& operator= ( const cfdNode& );

         ///Set the name of the node
         void SetName(std::string name);

         ///Toggle the display of this node on/off
         ///\param onOff Turn on/off rendering of this node\n
         ///Valid values are:\n
         ///ON == display this node\n
         ///OFF == hide this node\n
         void ToggleDisplay(std::string onOff);

         ///Toggle the display of this node on/off
         ///\param onOff Turn on/off rendering of this node\n
         void ToggleDisplay(bool onOff);

#ifdef _PERFORMER
         virtual pfNode* GetRawNode( void );
#elif _OSG
         virtual osg::Node* GetRawNode( void );
#elif _OPENSG
#endif

#ifdef _PERFORMER
         void pfTravNodeMaterial( pfNode* );
         void pfTravNodeFog( pfNode* node_1, pfFog* fog );
#elif _OSG
         void TravNodeMaterial(osg::Node*);
         void TravNodeFog(osg::Node* node_1, osg::Fog* fog);
#elif _OPENSG
#endif
         void SetNodeProperties( int, float, float* );
         void LoadFile( std::string
#ifdef _OSG
                       ,bool isStream=false
#endif
            );
         cfdNode* Clone( int );
   protected:
#ifdef _PERFORMER
         pfNode* _node;
         pfLightModel* lightModel;
#elif _OSG
         osg::ref_ptr<osg::Node> _node;
         osg::ref_ptr<osg::LightModel> lightModel;
#elif _OPENSG
#endif
         float op;
         float stlColor[ 3 ];
         int color;
         bool twosidedlighting;
   };
}
#endif
