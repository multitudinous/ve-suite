/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

#include "VETemplate.h"
#include <VE_Xplorer/SceneGraph/cfdGroup.h>
#include <VE_Xplorer/SceneGraph/cfdGeode.h>
#include <VE_Xplorer/SceneGraph/cfdDCS.h>
#include <VE_Xplorer/XplorerHandlers/cfdModel.h>
#include <VE_Xplorer/XplorerHandlers/cfdReadParam.h>
#include <VE_Xplorer/Utilities/fileIO.h>
#include <VE_Xplorer/XplorerHandlers/cfdFILE.h>
#include <VE_Xplorer/XplorerHandlers/cfdDataSet.h>
#include <VE_Conductor/Network/string_ops.h>
#include <VE_Xplorer/XplorerHandlers/cfdCursor.h>

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
VETemplate::VETemplate( void ) : cfdVEBaseClass()
{
  _objectName ="Template"; // Needs to match plugin name
}

// Destructor
VETemplate::~VETemplate( void )
{
}

void VETemplate::InitializeNode( cfdDCS* veworldDCS )
{
   cfdVEBaseClass::InitializeNode( veworldDCS );
}
