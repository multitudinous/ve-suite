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
#include <osgDB/FileUtils>
#include <osgDB/FileNameUtils>
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
   twosidedlighting = false;
}
////////////////////////////////////////////////////////////////////////////////
CADEntityHelper::CADEntityHelper( const CADEntityHelper& input )
{
#ifdef _OSG
   if( cadNode.valid() )
   {
      cadNode = input.cadNode;
   }
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
   }

   return *this;
}
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
   if ( !isStream )
   {
      if ( osgDB::getLowerCaseFileExtension(filename) == "osg" )
      {
         osgDB::ReaderWriter *rw = osgDB::Registry::instance()->getReaderWriterForExtension( osgDB::getLowerCaseFileExtension(filename) );
         if (!rw)
         {
            std::cerr << "Error: could not find a suitable reader/writer to load the specified file" << std::endl;
            return;
         }
         
         std::auto_ptr<progbuf> pb(new progbuf(osgDB::findDataFile(filename)));
         if (!pb->is_open())
         {
            std::cerr << "Error: could not open file `" << filename << "'" << std::endl;
            return;
         }
         
         std::cout << "Progress: ";
         
         std::istream mis(pb.get());
         osgDB::ReaderWriter::ReadResult rr = rw->readNode(mis);
         
         std::cout << std::endl;
         
         cadNode = rr.getNode();
         if (!cadNode.valid())
         {
            std::cerr << "Error: could not load file `" << filename << "'" << std::endl;
         }
      }
      else
      {
         cadNode = osgDB::readNodeFile(filename);
      }
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
