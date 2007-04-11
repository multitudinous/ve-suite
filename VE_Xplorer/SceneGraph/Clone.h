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
#ifndef CLONE_H
#define CLONE_H
/*!\file Clone.h
Clone API
*/

/*!\class VE_SceneGraph::Clone
*
*/
#include "VE_Xplorer/SceneGraph/DCS.h"

namespace VE_SceneGraph
{
	class DCS;
   class SceneNode;
}

namespace osg
{
   class Node;
}

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS Clone
{
public:
   Clone();
   //Clone( SceneNode* original );
	Clone( osg::Node* original );
	~Clone();

	void CloneNode( osg::Node* original );
   void SetTranslationArray(float* translation);
   void SetRotationArray(float* rotation);
   void SetScaleArray(float* scale);

   //returns the cloned structure including the
   //transform
	VE_SceneGraph::DCS* GetClonedGraph();
   osg::Node* Clone::CloneSubNode( osg::Node* node );
   
protected:
	osg::ref_ptr< VE_SceneGraph::DCS > cloneTransform;
};
}
#endif //CLONE_H
