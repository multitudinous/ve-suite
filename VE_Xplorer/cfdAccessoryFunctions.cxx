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
 * File:          $RCSfile: cfdAccessoryFunctions.cxx,v $
 * Date modified: $Date: 2004/03/23 16:29:12 $
 * Version:       $Revision: 1.2 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <cfdAccessoryFunctions.h>
#include <vtkDataArray.h>
//#include <vrj/Util/Debug.h>

cfdAccessoryFunctions::cfdAccessoryFunctions( )
{
}

cfdAccessoryFunctions::~cfdAccessoryFunctions( )
{
}

double cfdAccessoryFunctions::ComputeVectorMagnitude( double vectorComponents [ 3 ] )
{
   double magnitude = vectorComponents[ 0 ] * vectorComponents[ 0 ] +
                      vectorComponents[ 1 ] * vectorComponents[ 1 ] +
                      vectorComponents[ 2 ] * vectorComponents[ 2 ];
                      
   magnitude = sqrt( magnitude );
   return magnitude;
}

double * cfdAccessoryFunctions::ComputeVectorMagnitudeRange( vtkDataArray * dataArray )
{
   // magnitudeRange is used by vector-based visualizations when
   // "scale by vector magnitude" is selected. magnitudeRange[ 0 ] determines
   // a vector magnitude that will have zero length.  magnitudeRange[ 1 ]
   // determines a vector magnitude that corresponds to unit length. In this
   // function, we compute the lower and upper bounds on vector magnitude,
   // then we override the lower bound so that a zero length vector corresponds
   // only to a zero magnitude vector.

   if ( dataArray->GetNumberOfComponents() != 3 )
   {
      cerr << "ERROR: ComputeVectorMagnitudeRange requires 3-component "
           << "vector data" << std::endl;
      return NULL;
   }

   double * magnitudeRange = new double [ 2 ];

   int numTuples = dataArray->GetNumberOfTuples();

/*
   vprDEBUG(vprDBG_ALL,1) << "\tnum vector tuples = " << numTuples 
                          << std::endl << vprDEBUG_FLUSH;
*/

   // get the components of the first vector tuple...
   double vectorComponents[ 3 ];
   dataArray->GetTuple( 0, vectorComponents );

   // use this first tuple to initialize min and max range values
   magnitudeRange[ 0 ] = magnitudeRange[ 1 ] 
                       = ComputeVectorMagnitude( vectorComponents );
/*
   vprDEBUG(vprDBG_ALL,2) << " tuple 0, mag = " << magnitudeRange[ 0 ]
                          << std::endl << vprDEBUG_FLUSH;
*/

   // for all other vector tuples, compare to current magnitudeRange...
   for (int i = 1; i < numTuples; i++)
   {
      dataArray->GetTuple( i, vectorComponents );

      double magnitude = ComputeVectorMagnitude( vectorComponents );
/*
      vprDEBUG(vprDBG_ALL,3) << " tuple " << i << ", mag = " << magnitude
                             << std::endl << vprDEBUG_FLUSH;
*/
      if ( magnitudeRange[ 0 ] > magnitude )
         magnitudeRange[ 0 ] = magnitude;

      if ( magnitudeRange[ 1 ] < magnitude )
         magnitudeRange[ 1 ] = magnitude;
   }

   // override the minimum so that a zero-length vector corresponds
   // only to a zero-magnitude vector...
   magnitudeRange[ 0 ] = 0.0;
/*
   vprDEBUG(vprDBG_ALL,1) << "\tmagnitudeRange = "
      << magnitudeRange[ 0 ] << " : " << magnitudeRange[ 1 ]
      << std::endl << vprDEBUG_FLUSH;
*/

   return magnitudeRange;
}

