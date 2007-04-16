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
#include "VE_Xplorer/SceneGraph/CADEntityHelper.h"
#include "VE_Xplorer/SceneGraph/Group.h"
#include "VE_Xplorer/SceneGraph/Switch.h"
#include "VE_Xplorer/SceneGraph/Geode.h"

#include <iostream>
#include <sstream>
#include <string>
#include <fstream>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#ifdef _OSG
#include <osgDB/ReadFile>
#include <osg/Node>
#include <osg/Fog>
#include <osg/Group>
#include <osg/MatrixTransform>
#include <osg/Geode>
#include <osg/Sequence>
#include <osg/Material>
#include <osg/State>
#include <osg/StateSet>
#include <osg/StateAttribute>
#include <osg/ShadeModel>
#include <osgDB/Registry>
#include <osgDB/FileUtils>
#include <osg/Switch>
#include <osg/Group>
#include <osg/ShadeModel>
#include <osg/Geometry>
#include <osg/BlendFunc>
#include <osg/Array>
#include <osg/Depth>
#include <osg/LOD>
#include <osg/ShadeModel>
#include <osg/LightModel>
#include <osgDB/ReaderWriter>
#elif _OPENSG
#endif

#include <string>
#include <istream>
#include <sstream>

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
CADEntityHelper::CADEntityHelper()
{
   //biv--do we need to set type for scene node in here?
   //this->_group = new pfNode();
   op = 1.0f;
   stlColor[ 1 ] = stlColor[ 1 ] = stlColor[ 0 ] = -1;
   color = 0;
   twosidedlighting = false;

#ifdef _PERFORMER
   this->cadNode = 0;
   this->lightModel = 0;
#elif _OSG
   //cadNode = 0;//new osg::Node();
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
CADEntityHelper::CADEntityHelper( const CADEntityHelper& input )
{
   #ifdef _PERFORMER
   this->cadNode = input.cadNode;
   #elif _OSG
   if( cadNode.valid() )
   {
      cadNode = input.cadNode;
   }

   op = input.op;
   stlColor[0] = input.stlColor[0];
   stlColor[1] = input.stlColor[1];
   stlColor[2] = input.stlColor[2];
   color = input.color;

#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
CADEntityHelper& CADEntityHelper::operator=( const CADEntityHelper& input )
{
   if( this != &input ){

      #ifdef _PERFORMER
      pfDelete( this->cadNode );
      this->cadNode = input.cadNode;
      #elif _OSG
      //Recreate the node
      //cadNode->unref();
      cadNode = input.cadNode;
      #elif _OPENSG
      #endif

      op = input.op;
      stlColor[0] = input.stlColor[0];
      stlColor[1] = input.stlColor[1];
      stlColor[2] = input.stlColor[2];
      color = input.color;
   }

   return *this;
}

////////////////////////////////////////////////////////////////////////////////
// Code that can be used at a later date
// there are issues to be reolved on wether == should be defined
// as below 
/*bool Node::operator== ( const Node& node1 ) const
{
   if ( guid == node1.guid )
   {
      return true;
   }
   else
   {
      return false;
   }
}*/
////////////////////////////////////////////////////////////////////////////////
CADEntityHelper::~CADEntityHelper( void )
{
   // If neccesary
#ifdef _PERFORMER
   if ( this->cadNode != NULL )
   {
      vprDEBUG(vesDBG,3) << "destructor for Node " 
                              << std::endl << vprDEBUG_FLUSH;
      pfDelete( this->cadNode );
   }
#elif _OSG
   //cadNode->unref();
#elif _OPENSG
#endif
}
// Reimplement for other graphs
#ifdef _PERFORMER
pfNode* CADEntityHelper::GetNode( void )
#elif _OSG
osg::Node* CADEntityHelper::GetNode(void)
#elif _OPENSG
#endif
{
#ifdef _PERFORMER
   return cadNode;
#elif _OSG
   return cadNode.get();
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::SetName(std::string name)
{
   if ( GetNode() )
      GetNode()->setName(name.c_str());
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::ToggleDisplay(bool onOff)
{
   std::string value = (onOff==true)?"ON":"OFF";
   ToggleDisplay(value);
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::ToggleDisplay(std::string onOff)
{
   if ( !GetNode() )
      return;
      
   if(onOff == "ON")
   {
#ifdef _OSG
      GetNode()->setNodeMask(1);
#elif _OPENSG
#elif _PERFORMER
#endif
   }
   else if(onOff == "OFF")
   {
#ifdef _OSG
      GetNode()->setNodeMask(0);
#elif _OPENSG
#elif _PERFORMER
#endif
   }
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::LoadFile( std::string filename
#ifdef _OSG
                       ,bool isStream
#endif
                       )
{
   //std::ostringstream filestring;
   //filestring << filename;
   //if ( strstr( filestring.str().c_str(), ".stl") || strstr( filestring.str().c_str(), ".stla") )
   if ( strstr( filename.c_str(), ".stl") || strstr( filename.c_str(), ".stla") )

   {
      twosidedlighting = true;
   }

#ifdef _PERFORMER
   this->cadNode = pfdLoadFile( filename.c_str() ); 
   if ( twosidedlighting )
   {
      lightModel = new pfLightModel();
      lightModel->setLocal( PF_ON );
      lightModel->setTwoSide( PF_ON );
   }

#elif _OSG
   if(!isStream)
   {
      cadNode = osgDB::readNodeFile(filename);
   }
   else
   {
      std::istringstream textNodeStream(filename);
      cadNode = osgDB::Registry::instance()->getReaderWriterForExtension("osg")->readNode(textNodeStream).getNode();
   }
   if ( twosidedlighting && cadNode.valid() )
   {
      lightModel = new osg::LightModel;
      lightModel->setTwoSided( true );
      cadNode->getOrCreateStateSet()->setAttributeAndModes(lightModel.get(), osg::StateAttribute::ON);
   }
      
      
#elif _OPENSG
   std::cout << " Error:LoadFile !!! " << std::endl;
   exit( 1 );
#endif
#ifdef _PERFORMER
   if(cadNode){
#elif _OSG
   if(cadNode.valid()){
#endif
      cadNode->setName(filename.c_str());
   }
   else
   {
      std::cerr << "|\tERROR (CADEntityHelper::LoadFile) loading file name: " 
                  << filename << std::endl;
   }
}
////////////////////////////////////////////////////////////////////////////////
void CADEntityHelper::SetNodeProperties(int color,
                             float trans, 
                             float* stlColor )
{
   this->color = color;
   this->op = trans;
   this->stlColor[ 0 ] = stlColor[ 0 ];
   this->stlColor[ 1 ] = stlColor[ 1 ];
   this->stlColor[ 2 ] = stlColor[ 2 ];
}
