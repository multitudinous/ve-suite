/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: readWriteVtkThings.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef OPENVTKTHING_H
#define OPENVTKTHING_H

class vtkDataSet;
#include "VE_Installer/include/VEConfig.h"

namespace VE_Util
{
   VE_UTIL_EXPORTS void printWhatItIs( vtkDataSet * readerOutput );

   VE_UTIL_EXPORTS vtkDataSet * readVtkThing( std::string vtkFilename, int printFlag = 0 );  //default is not to print information

   VE_UTIL_EXPORTS bool writeVtkThing( vtkDataSet * vtkThing, std::string vtkFilename, int binaryFlag = 0 );// default is to print ascii file

   VE_UTIL_EXPORTS void printBounds( double bounds[6] );
}
#endif
