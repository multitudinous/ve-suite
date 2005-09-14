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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/VE_UI/UI_ModelData.h"
#include <iostream>

#include <wx/string.h>

using namespace std;

UI_ModelData::UI_ModelData( VjObs_ptr ref )
{
   server_ref = VjObs::_duplicate(ref);
   
   if ( !CORBA::is_nil( server_ref.in() ) )
   {
      try
      {
         _models = server_ref->GetModels();
      } 
      catch ( CORBA::Exception& ) 
      {
         std::cout << " could not connect to ve " << std::endl;
         return;
      }
   }
   else
   {
      std::cerr << " ERROR : App won't run " << std::endl;
      return;
   }

   for ( unsigned int i = 0; i < _models->length(); ++i )
   {
      std::vector< int > temp;
      geomControl.push_back( temp );
      CORBA::ULong j = i;
      CORBA::ULong k = _models[ j ].geometrynames.length();
      for ( unsigned int l = 0; l < k; ++l )
      {
        geomControl.back().push_back( 1 );
      }
   }
}

UI_ModelData::~UI_ModelData( void )
{
}

wxString UI_ModelData::GetModelName( int input )
{
   //CORBA::ULong i = input;
   wxString dummy; 
   dummy<<(input);
   wxString _modelName = wxT("model_ ") + dummy;
   return _modelName;
   //return &(_models[ i ].modelname);
}

short UI_ModelData::GetNumberOfModels( void )
{
   return (_models)->length();
}

short UI_ModelData::GetNumberOfGeomFiles( int input )
{
   CORBA::ULong i = input;
   return _models[ i ].geometrynames.length();
}

VjObs::scalar_p*  UI_ModelData::GetGeomFilenames( int input)
{
   CORBA::ULong i = input;
   return &(_models[ i ].geometrynames);
}

short UI_ModelData::GetNubmerofDataSets( int input )
{
   CORBA::ULong i = input;
   return _models[ i ].dataVector.length();
}
/*
VjObs::scalar_p*  UI_ModelData::GetScalarNames( int input )
{
   CORBA::ULong i = input;
   return &(_models[ i ].scalarnames);
}

VjObs::scalar_p*  UI_ModelData::GetVectorNames( int input )
{
   CORBA::ULong i = input;
   return &(_models[ i ].vectornames);
}

VjObs::scalar_p*  UI_ModelData::GetDataSetNames( int input )
{
   CORBA::ULong i = input;
   return &(_models[ i ].datasetnames);
}
*/
VjObs::obj_p*     UI_ModelData::GetDataSetTypes( int input )
{
   CORBA::ULong i = input;
   return &(_models[ i ].datasettypes);
}

VjObs::obj_p*     UI_ModelData::GetNumberOfScalarsPerDataSet( int input )
{
   CORBA::ULong i = input;
   return &(_models[ i ].num_scalars_per_dataset);
}

VjObs::obj_p*     UI_ModelData::GetNumberOfVectorsPerDataSet( int input )
{
   CORBA::ULong i = input;
   return &(_models[ i ].num_vectors_per_dataset);
}

VjObs::Datasets*  UI_ModelData::GetDataSets( int input )
{
   CORBA::ULong i = input;
   return &(_models[ i ].dataVector);
}

std::vector< int* > UI_ModelData::GetGeometryFileSettings( unsigned int input )
{
   std::vector< int* > temp;
   for ( unsigned int i = 0; i < geomControl[ input ].size(); ++i )
   {
      //int test = (int)geomControl[ input ].at( i );
      temp.push_back( &geomControl[ input ].at( i ) );
   }
   return temp; 
}

