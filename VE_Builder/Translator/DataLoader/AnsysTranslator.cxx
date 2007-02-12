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
 * Date modified: $Date: 2006-08-19 20:42:23 -0500 (Sat, 19 Aug 2006) $
 * Version:       $Rev: 5225 $
 * Author:        $Author: sgent $
 * Id:            $Id: StarCDTranslator.cxx 5225 2006-08-20 01:42:23Z sgent $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Builder/Translator/DataLoader/AnsysTranslator.h"
#include "VE_Builder/Translator/DataLoader/ansysReader.h"
#include <vtkDataSet.h>
#include <vtkUnstructuredGrid.h>

#include <iostream>

using namespace VE_Builder;
////////////////////////////////////////
//Constructors                        //
////////////////////////////////////////
AnsysTranslator::AnsysTranslator()
{

   SetTranslateCallback(&ansysToVTK);
   SetPreTranslateCallback(&_cmdParser);
}
/////////////////////////////////////////
AnsysTranslator::~AnsysTranslator()
{

}
//////////////////////////////////////////////////////////////////////////
void AnsysTranslator::AnsysPreTranslateCbk::Preprocess(int argc,char** argv,
                                               VE_Builder::cfdTranslatorToVTK* toVTK)
{
   PreTranslateCallback::Preprocess( argc, argv, toVTK );
}
////////////////////////////////////////////////////////////////////////////////
void AnsysTranslator::AnsysTranslateCbk::Translate(vtkDataObject*& outputDataset,
		                                     VE_Builder::cfdTranslatorToVTK* toVTK)
{
   VE_Builder::AnsysTranslator* ansysTransVTK =
              dynamic_cast<VE_Builder::AnsysTranslator*>(toVTK);
   if ( ansysTransVTK )
   {
      ansysReader* ansys = new ansysReader( ansysTransVTK->GetFile(0).c_str() );
      
      if ( !outputDataset )
      {
         outputDataset = vtkUnstructuredGrid::New();
      }
      outputDataset->ShallowCopy( ansys->GetUGrid() );
      delete ansys;
      outputDataset->Update();
   }
}
////////////////////////////////////////////////////////////////////////////////
void AnsysTranslator::DisplayHelp( void )
{
   std::cout << "|\tAnsys Translator Usage:" << std::endl
               << "\t -singleFile <rst_filename_to_load> -o <output_dir> "
               << "-outFileName <output_filename> -loader rst -w file" << std::endl;
}

