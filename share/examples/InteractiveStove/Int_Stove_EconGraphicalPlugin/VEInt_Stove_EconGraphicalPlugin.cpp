/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdVEBaseClass.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VEInt_Stove_EconGraphicalPlugin.h"
#include "VE_Xplorer/SceneGraph/Group.h"
#include "VE_Xplorer/SceneGraph/Geode.h"
#include "VE_Xplorer/SceneGraph/DCS.h"
#include <VE_Xplorer/XplorerHandlers/cfdModel.h>
#include <VE_Xplorer/XplorerHandlers/cfdReadParam.h>
#include "VE_Xplorer/Utilities/fileIO.h"
//#include "VE_Xplorer/cfdFILE.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Conductor/Network/string_ops.h"
#include "VE_Xplorer/XplorerHandlers/cfdCursor.h"

#include <fstream>
//#include <cstdlib>
#include <string>
#include <map>

#include <vrj/Util/Debug.h>

#include <vtkSphereSource.h>
#include <vtkPolyDataNormals.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkProperty.h>

using namespace std;
using namespace VE_Xplorer;
using namespace VE_SceneGraph;

// Constructor
VEInt_Stove_EconGraphicalPlugin::VEInt_Stove_EconGraphicalPlugin( void ) : cfdVEBaseClass()
{
  _objectName ="Int_Stove_Econ"; // Needs to match plugin name
   //_onSceneGraph = false;
   //_param = NULL;
}

// Destructor
VEInt_Stove_EconGraphicalPlugin::~VEInt_Stove_EconGraphicalPlugin( void )
{
   //if ( _param )
   //   delete [] _param;
}

void VEInt_Stove_EconGraphicalPlugin::InitializeNode( osg::Group* veworldDCS )
{
   cfdVEBaseClass::InitializeNode( veworldDCS );
   this->_param = new char[100];
   //strcpy( this->_param, "./VEInt_Stove_EconGraphicalPlugin.param");
   //cout << _param << endl;
   //CreateObjects();
}

