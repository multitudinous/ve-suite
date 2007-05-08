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
*/

/*!\class VE_SceneGraph::Clone
*
*/

/*!\namespace VE_SceneGraph
*
*/

// --- VE-Suite Includes --- //
#include "VE_Xplorer/SceneGraph/DCS.h"

namespace VE_SceneGraph
{
   class DCS;
   class SceneNode;
}

// --- OSG Includes --- //
namespace osg
{
   class Node;
}

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS Clone
{
public:
   ///Base Constructor
   Clone( void );

   ///Constructor
   ///\param original The original node to be cloned
   Clone( osg::Node* original );

   //Destructor
   ~Clone( void );

   ///Clones a scenegraph node
   ///\param original The original node to be cloned
   void CloneNode( osg::Node* original );

   ///Set the translation array of the clone
   ///\param translation The translation array pointer
   void SetTranslationArray( float* translation );

   ///Set the rotation array of the clone
   ///\param rotation The rotation array pointer
   void SetRotationArray( float* rotation );

   ///Set the scale array of the clone
   ///\param scale The scale array pointer
   void SetScaleArray( float* scale );

   ///Return the cloned structure including the transform
   VE_SceneGraph::DCS* GetClonedGraph( void );

   ///
   ///\param node
   osg::Node* CloneSubNode( osg::Node* node );
   
protected:
   osg::ref_ptr< VE_SceneGraph::DCS > cloneTransform;///<The cloned structure including the transform

};
}

#endif //CLONE_H
