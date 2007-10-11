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

#ifndef _DATA_LOADER_H_
#define _DATA_LOADER_H_
/*!\file DataLoader.h
  *Data Loader API
  */
/*!\class VE_Builder::DataLoader
 * This is an interface that allows you to load data into tool 
 * in VE-Suite.
 */
#include <ves/VEConfig.h>
#include <string>
#include <map>

class vtkDataObject;

namespace VE_Builder
{
   class cfdTranslatorToVTK;
}

namespace VE_Builder
{
class VE_USER_BUILDER_EXPORTS DataLoader
{
public:
   ///Constructor
   DataLoader();
   ///Destructor
   ~DataLoader();
private:
   ///Copy Constructor
   DataLoader( const DataLoader& );
   ///equal operator
   DataLoader& operator= ( const DataLoader& );
public:   
   ///Get the active translator for the data that is loaded.
   cfdTranslatorToVTK* GetActiveTranslator( void );

   ///Set the data file name to be translated.
   ///\param inputData The file name of the data to be loaded.
   void SetInputData( std::string inputData, std::string inputDir );

   ///Get the vtkDataSet for the data that was passed in
   ///\param inputData The file name of the data to be loaded.
   vtkDataObject* GetVTKDataSet( int argc, char** argv );

private:
   std::string inputDataName;///<The name of the data file.
   std::string inputDataDir;///<The name of the input data dir.
   cfdTranslatorToVTK* activeLoader;///<The pointer to the active loader
   std::map< std::string, cfdTranslatorToVTK* > translatorMap;///<Map the translators to a filename or extension, must be unique
};
}
#endif// _DATA_LOADER_H_
