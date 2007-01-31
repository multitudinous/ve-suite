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
#ifndef VE_GROUP_H
#define VE_GROUP_H
/*!\file Group.h
Group API
*/

/*!\class VE_SceneGraph::Group
*
*/
#include "VE_Xplorer/SceneGraph/SceneNode.h"

#ifdef _PERFORMER
#include <pf/pfGroup.h>;
#elif _OSG
#include <osg/Group>
#elif _OPENSG
#endif

namespace VE_SceneGraph{
#ifdef _OSG
class VE_SCENEGRAPH_EXPORTS Group : public osg::Group, public SceneNode
#elif _PERFORMER
class VE_SCENEGRAPH_EXPORTS Group : public pfGroup
#endif
{
public:
   Group();
   Group( const Group& );

   virtual ~Group( void );

   Group& operator=( const Group& );




   //virtual Node* Clone( int );


protected:


};
}

#endif //VE_GROUP_H
