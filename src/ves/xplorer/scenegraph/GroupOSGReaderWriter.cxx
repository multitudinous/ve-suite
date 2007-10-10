/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#include "VE_Xplorer/SceneGraph/Group.h"

#include <osg/Group>
#include <osg/Node>
#include <osgDB/Registry>
#include <osgDB/Input>
#include <osgDB/Output>

//C/C++ Libraries
#include <iostream>
#include <algorithm>
#include <string>

using namespace VE_SceneGraph;
// forward declare functions to use later.
bool VEGroup_readLocalData(osg::Object& obj, osgDB::Input& fr);
bool VEGroup_writeLocalData(const osg::Object& obj, osgDB::Output& fw);

// register the read and write functions with the osgDB::Registry.
osgDB::RegisterDotOsgWrapperProxy ve_GroupProxy
(
    new VE_SceneGraph::Group,
    "Group",
    "Object Node VE_SceneGraph::Group",
    &VEGroup_readLocalData,
    &VEGroup_writeLocalData
);
//////////////////////////////////////////////////////////////
bool VEGroup_readLocalData(osg::Object& obj, osgDB::Input& fr)
{
    bool iteratorAdvanced = false;

    VE_SceneGraph::Group& group = static_cast<VE_SceneGraph::Group&>(obj);

    int num_children;
    if (fr[0].matchWord("num_children") &&
        fr[1].getInt(num_children))
    {
        // could allocate space for children here...
        fr+=2;
        iteratorAdvanced = true;
    }

    osg::Node* node = NULL;
    while((node=fr.readNode())!=NULL)
    {
        group.addChild(node);
        iteratorAdvanced = true;
    }

    return iteratorAdvanced;
}

///////////////////////////////////////////////////////////
bool VEGroup_writeLocalData(const osg::Object& obj, osgDB::Output& fw)
{
   ///call the base class writer
   const VE_SceneGraph::Group& group = static_cast<const VE_SceneGraph::Group&>(obj);
    //fw.writeObject(group);
    fw.indent() << "num_children " << group.getNumChildren() << std::endl;
    for(unsigned int i=0;i<group.getNumChildren();++i)
    {
        fw.writeObject(*group.getChild(i));
    }
    return true;
}
