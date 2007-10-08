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
#include <iostream>

using namespace VE_Util;
//////////////////////////////////////////////////////////////////
ProcessScalarRangeCallback::ProcessScalarRangeCallback()
{
}
///////////////////////////////////////////////////////////////////
ProcessScalarRangeCallback::~ProcessScalarRangeCallback()
{
    std::map<std::string, double* >::iterator iter;
    for(iter == m_scalarRanges.begin();
        iter != m_scalarRanges.end(); 
        ++iter)
    {
        delete (*iter).second;
    }
    m_scalarRanges.clear();
}
//////////////////////////////////////////////////////////////////////////////////////////
void ProcessScalarRangeCallback::OperateOnDataset(vtkDataSet* dataset)
{
    // store actual range...
    int ii = 0;
    double scalarRange[2] = {100000.,-10000.};
                
    std::map<std::string, double* >::iterator scalarRangeInfo;
    for ( int i=0; i < dataset->GetPointData()->GetNumberOfArrays(); ++i )
    {
        vtkDataArray* array = dataset->GetPointData()->GetArray(i);
        if (array->GetNumberOfComponents() != 1 )
        {
            continue;
        }
        //Not already in the list so find the range
        //This assumes that there are only unique
        //scalars AND that each dataset has the same
        //unique scalars---This may be incorrect because
        //each dataset in the multiblock may have it's own scalar range but
        //it's not clear if that is the case...
        scalarRangeInfo = m_scalarRanges.find(array->GetName());
        if( scalarRangeInfo == m_scalarRanges.end() )
        {
            m_scalarRanges[array->GetName()] = new double[2];
            array->GetRange( m_scalarRanges[array->GetName()] );
        }
    }
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////
void ProcessScalarRangeCallback::GetScalarRange(std::string scalarName,double range[])
{
    try
    {
        range[0] = m_scalarRanges[scalarName][0];
        range[1] = m_scalarRanges[scalarName][1];
    }
    catch(...)
    {
        std::cout<<"Invalid scalar specified: "<<scalarName<<std::endl;
        std::cout<<"ProcessScalarRangeCallback::GetScalarRange"<<std::endl;
    }
}

