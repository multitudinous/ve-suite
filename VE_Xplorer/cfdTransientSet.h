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
 * File:          $RCSfile: cfdTransientSet.h,v $
 * Date modified: $Date: 2004/03/23 16:29:19 $
 * Version:       $Revision: 1.6 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_TRANSIENT_SET_H
#define CFD_TRANSIENT_SET_H

#include <vector>

class pfDCS;
class cfdReadParam;
class vtkDataSet;

class cfdTransientSet
{
   public:
      // the constructor allows transientInfo to pass on the DCS
      cfdTransientSet( char * dir, int id, pfDCS * dcs );
      ~cfdTransientSet( );

      char * GetDirectory();
      //void   SetDirectory( char * );

      int    GetID();
      //void   SetID( int );

      // the following function allows transientInfo to pass on the DCS
      pfDCS * GetDCS();
      //void    SetDCS( pfDCS * );
      
      double * GetMinMax( int scalarIndex );
      void     SetMinMax( int scalarIndex, double * minMax );

      int GetNumberOfScalars();
      int GetNumberOfVectors();

      int GetNumberOfDataSets();

      void ReadScalarRanges();

      void SetParameterFile( cfdReadParam* param );

      void RecheckDatasetTypes();

   private:
      void CountScalarsAndVectors( vtkDataSet * dataset, 
                                   int & numScalars, int & numVectors );

      char * GetFrameFileName( int );
      std::vector< char* > frameFileNames;
      int * order;

      char         * directory;
      int            id;

      double      ** min_max;
      int            activeScalar;
      int            numScalars;
      int            numVectors;
      char        ** scalarName;
      //char        ** vectorName;
      pfDCS        * dcs;
      cfdReadParam * param;
      int            numFiles;
      int            iii;     // start index
};

#endif
