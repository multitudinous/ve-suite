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
#include "VE_Builder/Translator/DataLoader/StarCDTranslator.h"
#include "VE_Builder/Translator/DataLoader/starReader.h"
#include <vtkDataSet.h>
#include <vtkUnstructuredGrid.h>
//#include <vtkCellDataToPointData.h>
//#include <vtkPointData.h>

#include <iostream>

using namespace VE_Builder;
////////////////////////////////////////
//Constructors                        //
////////////////////////////////////////
StarCDTranslator::StarCDTranslator()
{

   SetTranslateCallback(&starToVTK);
   SetPreTranslateCallback(&_cmdParser);
}
/////////////////////////////////////////
StarCDTranslator::~StarCDTranslator()
{

}
//////////////////////////////////////////////////////////////////////////
void StarCDTranslator::StarCDPreTranslateCbk::Preprocess(int argc,char** argv,
                                               VE_Builder::cfdTranslatorToVTK* toVTK)
{
   PreTranslateCallback::Preprocess( argc, argv, toVTK );
}
////////////////////////////////////////////////////////////////////////////////
void StarCDTranslator::StarCDTranslateCbk::Translate(vtkDataSet*& outputDataset,
		                                     VE_Builder::cfdTranslatorToVTK* toVTK)
{
   VE_Builder::StarCDTranslator* starCDToVTK =
              dynamic_cast<VE_Builder::StarCDTranslator*>(toVTK);
   if ( starCDToVTK )
   {
      starReader* star = new starReader( starCDToVTK->GetFile(0).c_str() );
      star->ReadParameterFile();
      
      if ( !outputDataset )
      {
         outputDataset = vtkUnstructuredGrid::New();
      }
      outputDataset->ShallowCopy( star->GetUnsGrid() );
      delete star;
      outputDataset->Update();
   }
}
////////////////////////////////////////////////////////////////////////////////
void StarCDTranslator::DisplayHelp( void )
{
   std::cout << "|\tStarCD Translator Usage:" << std::endl
               << "\t -singleFile <filename_to_load> -o <output_dir> "
               << "-outFileName <output_filename> -loader star -w file" << std::endl;
}

