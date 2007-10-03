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
 * Date modified: $Date: 2007-06-15 11:06:13 -0500 (Fri, 15 Jun 2007) $
 * Version:       $Rev: 8206 $
 * Author:        $Author: biv $
 * Id:            $Id: ComputeDataObjectBoundsCallback.h 9200 2007-10-03 19:17:07Z biv $
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/Utilities/ProcessScalarRangeCallback.h"
#include <vtkDataSet.h>
#include <vtkPointData.h>

using namespace VE_Util;
//////////////////////////////////////////////////////////////////
ProcessScalarRangeCallback::ProcessScalarRangeCallback()
{

}
///////////////////////////////////////////////////////////////////
ProcessScalarRangeCallback::~ProcessScalarRangeCallback()
{
    m_scalarRanges.clear();
}
///////////////////////////////////////////////////////////////////////////
void ProcessScalarRangeCallback::OperateOnDataset(vtkDataSet* dataset)
{
    // store actual range...
    int ii = 0;
    double scalarRange[2] = {100000,-10000};
    for ( int i=0; i < dataset->GetPointData()->GetNumberOfArrays(); ++i )
      {
          vtkDataArray* array = dataset->GetPointData()->GetArray(i);
          if (array->GetNumberOfComponents() != 1 )
          {
             continue;
          }
          
         
         array->GetRange(  );

         vprDEBUG(vesDBG,1) << "\tarray(" << i << "), scalarName[" 
            << ii << "] = \"" << this->scalarName[ ii ] 
            << "\", actualScalarRange = "
            << this->actualScalarRange[ ii ][ 0 ] << " : "
            << this->actualScalarRange[ ii ][ 1 ]
            << std::endl << vprDEBUG_FLUSH;

         this->AutoComputeUserRange( 
                              this->GetParent()->GetActualScalarRange( ii ),
                              this->displayedScalarRange[ ii ] );

         vprDEBUG(vesDBG,1) << "|\tarray(" << i << "), scalarName[" 
            << ii << "] = \"" << this->scalarName[ ii ] 
            << "\", displayedScalarRange = "
            << this->displayedScalarRange[ ii ][ 0 ] << " : "
            << this->displayedScalarRange[ ii ][ 1 ]
            << std::endl << vprDEBUG_FLUSH;

         ii++;
      }
   }
}
//////////////////////////////////////////////////////////////////////////////////    
unsigned int ProcessScalarRangeCallback::GetNumberOfParameters(bool isVector)
{
   return (isVector)?m_numberOfParameters[1]:m_numberOfParameters[0];
}

