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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VEAdiabaticFlameTempGraphicalPlugin.h"
#include "VE_SceneGraph/cfdGroup.h"
#include "VE_SceneGraph/cfdGeode.h"
#include "VE_SceneGraph/cfdDCS.h"
#include "VE_Xplorer/cfdModel.h"
#include "VE_Xplorer/cfdReadParam.h"
#include "VE_Xplorer/fileIO.h"
#include "VE_Xplorer/cfdFILE.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Conductor/Framework/string_ops.h"
#include "VE_Xplorer/cfdCursor.h"

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
VEAdiabaticFlameTempGraphicalPlugin::VEAdiabaticFlameTempGraphicalPlugin( void ) : cfdVEBaseClass()
{
  _objectName ="AdiabaticFlameTemp"; // Needs to match plugin name
   //_onSceneGraph = false;
  _param.clear();
}

// Destructor
VEAdiabaticFlameTempGraphicalPlugin::~VEAdiabaticFlameTempGraphicalPlugin( void )
{
   if ( !_param.empty() )
      _param.clear();
}

void VEAdiabaticFlameTempGraphicalPlugin::InitializeNode( osg::Group* veworldDCS )
{
   std::cout<<"Initializing NODE!!!!!!!!!!!!!!"<<std::endl;
   cfdVEBaseClass::InitializeNode( veworldDCS );
   _param.assign( "./Plugins/vrxpr.param" );
   std::cout<<"Calling CreateObjects!!!!!!!!!!!!!!"<<std::endl;
   CreateObjects();
}

