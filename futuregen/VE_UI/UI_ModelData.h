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
#ifndef UI_MODELDATA_H
#define UI_MODELDATA_H

#include "VjObsS.h"

class UI_ModelData
{
   public:
      UI_ModelData( VjObs_ptr );
      ~UI_ModelData( void );
      //UI_ModelData( const UI_ModelData& );
      //UI_ModelData& operator=( const UI_ModelData& );
      
      short GetNumberOfGeomFiles( int );
      VjObs::scalar_p*  GetGeomFilenames( int );
      short GetNubmerofDataSets( int );
      VjObs::scalar_p*  GetScalarNames( int );
      VjObs::scalar_p*  GetVectorNames( int );
      VjObs::scalar_p*  GetDataSetNames( int );
      VjObs::obj_p*     GetDataSetTypes( int );
      VjObs::obj_p*     GetNumberOfScalarsPerDataSet( int );
      VjObs::obj_p*     GetNumberOfVectorsPerDataSet( int );
      
   private:
      VjObs::Models_var _models;
      VjObs_var server_ref;
};

#endif
