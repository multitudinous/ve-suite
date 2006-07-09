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
#ifndef CFD_SWITCH_H
#define CFD_SWITCH_H

#include "VE_Xplorer/SceneGraph/cfdGroup.h"

#ifdef _PERFORMER
class pfSwitch;
#elif _OSG
#include <osg/Switch>
#elif OPENSG
#endif

namespace VE_SceneGraph{

   class VE_SCENEGRAPH_EXPORTS cfdSwitch : public cfdGroup
   {
      public:
         cfdSwitch();
         cfdSwitch(const cfdSwitch& cSwitch);
         virtual ~cfdSwitch();

         enum Value{OFF=-1,ON};

         //probably need more functions but
         //this is all we need for now
         //the rest are inherited from group
         void SetVal(int whichChildIsActive);
         int RemoveChild( cfdNode* child );
         int AddChild(cfdNode* child);
         int ReplaceChild(cfdNode* old,cfdNode* newNode);
         int GetNumChildren();
         void InsertChild(int index,cfdNode* node);
   
#ifdef _PERFORMER
         pfNode* GetRawNode( void );
#elif _OSG
         osg::Node* GetRawNode( void );
#elif _OPENSG
#endif
         cfdSwitch& operator=(const cfdSwitch& rhs);
   protected:
#ifdef _PERFORMER
      pfSwitch* _switch;
#elif _OSG
      osg::ref_ptr<osg::Switch> _switch;
#elif _OPENSG
#endif
   };
}
#endif// CFD_SWITCH_H
