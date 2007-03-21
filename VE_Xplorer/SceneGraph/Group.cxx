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
#include "VE_Xplorer/SceneGraph/Group.h"

#ifdef _PERFORMER
#include <Performer/pf/pfGroup.h>
#include <Performer/pf/pfNode.h>
#elif _OSG
#include <osg/Group>
#include <osg/Node>
#include <osgDB/Registry>
#include <osgDB/Input>
#include <osgDB/Output>
#elif _OPENSG
#endif

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
    "Object Node Group VE_SceneGraph::Group",
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

////////////////////////////////////////////////////////////////////////////////
Group::Group(const Group& group,const osg::CopyOp& copyop):
osg::Group(group,copyop)
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
Group::Group( void )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
Group::~Group( void )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
int Group::RemoveChild( SceneNode* child )
{
#ifdef _OPENSG
   cerr << " ERROR: Group::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
	return this->removeChild( dynamic_cast< osg::Node* >( child ));
#endif
}
////////////////////////////////////////////////////////////////////////////////
int Group::AddChild( SceneNode* child )
{
#ifdef _OPENSG
   cerr << " ERROR: Group::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
   return this->addChild( dynamic_cast< Node* >( child ));
#endif
   
}
////////////////////////////////////////////////////////////////////////////////
void Group::InsertChild( int position, SceneNode* child )
{
#ifdef _OPENSG
   cerr << " ERROR: Group::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
   this->insertChild( position, dynamic_cast< Node* >( child ));
#endif
}
////////////////////////////////////////////////////////////////////////////////
int Group::GetNumChildren( void )
{
#ifdef _OPENSG
   cerr << " ERROR: Group::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
   return this->getNumChildren();
#endif
}
////////////////////////////////////////////////////////////////////////////////
const std::string Group::GetName( void )
{
#ifdef _OPENSG
   return 0;
#endif
#ifdef _PERFORMER
   return _Group->getName();
#elif _OSG
   return this->getName().data();
#endif
}
////////////////////////////////////////////////////////////////////////////////
void Group::SetName( std::string name )
{
#ifdef _OPENSG
   std::cerr << " ERROR: Group::SetName is NOT implemented " << std::endl;
   exit( 1 );
#endif
#ifdef _PERFORMER
   _Group->setName( name.c_str() );
#elif _OSG
   this->setName( name );
#endif
}
////////////////////////////////////////////////////////////////////////////////
int Group::ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild )
{
#ifdef _OPENSG
   cerr << " ERROR: Group::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
   return this->replaceChild( dynamic_cast< Node* >( childToBeReplaced ), dynamic_cast< Node* >( newChild ) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
bool Group::SearchChild( VE_SceneGraph::SceneNode* searchChild )
{
#ifdef _OPENSG
	
#elif _OSG
	return this->containsNode( dynamic_cast< osg::Node* >(searchChild) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* Group::GetParent( unsigned int position )
{
#ifdef _OPENSG
   
#elif _OSG
	return this->getParent( position );
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* Group::GetChild( unsigned int position )
{
#ifdef _OPENSG
   
#elif _OSG
	return this->getChild( position );
#endif
}
////////////////////////////////////////////////////////////////////////////////
void Group::ToggleDisplay(bool onOff)
{
   std::string value = (onOff==true)?"ON":"OFF";
   ToggleDisplay(value);
}
////////////////////////////////////////////////////////////////////////////////
void Group::ToggleDisplay(std::string onOff)
{
   if(onOff == "ON")
   {
#ifdef _OSG
      this->setNodeMask(1);
#elif _OPENSG
#elif _PERFORMER
#endif
   }
   else if(onOff == "OFF")
   {
#ifdef _OSG
      this->setNodeMask(0);
#elif _OPENSG
#elif _PERFORMER
#endif
   }
}
////////////////////////////////////////////////////////////////////////////////
