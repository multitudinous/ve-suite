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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "UI_ModelData.h"
#include <iostream>

using namespace std;

UI_ModelData::UI_ModelData( VjObs_ptr ref )
{
   server_ref = VjObs::_duplicate(ref);
   
   if ( !CORBA::is_nil( server_ref.in() ) )
   {
      _models = server_ref->GetModels();
   }
   else
   {
      cerr << " ERROR : App won't run " << endl;
      return;
   }
}

UI_ModelData::~UI_ModelData( void )
{
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
   cout<<input<<" : "<<_models->length();
   CORBA::ULong i = input;
   return _models[ i ].datasetnames.length();
}

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

