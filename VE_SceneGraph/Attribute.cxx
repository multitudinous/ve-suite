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
#include "VE_SceneGraph/Attribute.h"

#ifdef _PERFORMER
#include <Performer/pf.h>
#include <Performer/pf/pfNode.h>
//Performer static member for performer compliance
//it allows performer to determine the class type

pfType* VE_SceneGraph::Utilities::Attribute::_classType = NULL;
//initialize our class w/ performer at run time
void VE_SceneGraph::Utilities::Attribute::init(void)
{
   if(_classType == 0)
   {
      //initialize the parent
      pfGeoState::init();
      //create the new class type
      _classType = new pfType(pfGeoState::getClassType(),"Attribute");
   }
}
#endif

using namespace VE_SceneGraph::Utilities;
using namespace VE_CAD;
//////////////////////
///Constructor      //
//////////////////////
Attribute::Attribute()
#ifdef _OSG
:osg::StateSet()
#elif _PERFORMER
:pfGeoState()
#endif
{
#ifdef _PERFORMER
   init();
   setType(_classType);
#endif

}
#ifdef _OSG
//Copy Constructor for OpenSceneGraph object
//////////////////////////////////////////////////////////
Attribute::Attribute(const Attribute& veAttribute,
                     const osg::CopyOp& copyop)
:osg::StateSet(veAttribute,copyop)
{
}
#endif
///////////////////////
Attribute::~Attribute()
{
}
////////////////////////////////////////////////////////////////////////////
void Attribute::CreateStateSetFromAttribute(VE_CAD::CADAttribute* attribute)
{
}
